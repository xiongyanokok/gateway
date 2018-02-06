package com.hexun.gateway.aggregator;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.beetl.core.Configuration;
import org.beetl.core.GroupTemplate;
import org.beetl.core.Template;
import org.beetl.core.resource.StringTemplateResourceLoader;
import org.redisson.api.RBucket;
import org.springframework.beans.factory.annotation.Autowired;

import com.dianping.cat.Cat;
import com.dianping.cat.message.Message;
import com.dianping.cat.message.Transaction;
import com.fasterxml.jackson.core.type.TypeReference;
import com.hexun.cache.IRedisClient;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.common.AggregatorUtils;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.enums.MethodEnum;
import com.hexun.gateway.pojo.ResourceInfo;

import lombok.extern.slf4j.Slf4j;

/**
 * 聚合请求抽象类
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:55:24
 */
@Slf4j
public abstract class AbstractAggregatorRequest<T> implements AggregatorRequest<T> {

	/**
	 * IRedisClient
	 */
	@Autowired
	private IRedisClient redisClient;
	
	/**
     * GroupTemplate
     */
    private GroupTemplate groupTemplate;
    
    /**
     * 初始化GroupTemplate
     * 
     * @throws IOException
     */
	@PostConstruct
    public void init() throws IOException {
        StringTemplateResourceLoader resourceLoader = new StringTemplateResourceLoader();
        Configuration cfg = Configuration.defaultConfiguration();
        groupTemplate = new GroupTemplate(resourceLoader, cfg);
    }
	
	
	/**
	 * 并行
	 * 
	 * @param resources
	 * @return
	 */
	@Override
	public String parallel(List<ResourceInfo> resources) {
		Map<String, String> resultMap = new HashMap<>(resources.size());
		Map<ResourceInfo, T> futureMap = new HashMap<>(resources.size());
		for (ResourceInfo resource : resources) {
			String value = getCacheResult(resource);
			if (StringUtils.isNotEmpty(value)) {
				resultMap.put(resource.getResourceName(), value);
			} else {
				futureMap.put(resource, execute(resource));
			}
		}

		for (Map.Entry<ResourceInfo, T> entry : futureMap.entrySet()) {
			ResourceInfo resource = entry.getKey();
			T t = entry.getValue();
			// 获取执行结果
			String value = getResult(resource, t);
			resultMap.put(resource.getResourceName(), value);
		}
		
		StringBuilder result = new StringBuilder();
		for (Map.Entry<String, String> entry : resultMap.entrySet()) {
			if (result.length() > 0) {
				result.append(",");
			}
			result.append("\"" + entry.getKey() + "\":" + entry.getValue());
		}
		result.insert(0, "{").append("}");
		return result.toString();
	}
	
	/**
	 * 串行
	 * 
	 * @param resources
	 * @return
	 */
	@Override
	public String serial(List<ResourceInfo> resources) {
		// 排序
        Collections.sort(resources);
        // 串行
        String result = serialWorker(resources.iterator(), null);
        if (StringUtils.isEmpty(result)) {
        	result = "{}";
        }
        return result;
	}
	
	/**
	 * 从缓存中获取信息
	 *
	 * @param resource
	 * @return
	 */
	public String getCacheResult(ResourceInfo resource) {
		if (!resource.getIsCache()) {
			return null;
		}
		// 缓存key
		String key = redisKey(resource);
		RBucket<String> rBucket = redisClient.getBucket(key);
		return rBucket.get();
	}
	
	/**
	 * 执行http请求
	 * 
	 * @param resource
	 * @return
	 */
	public T execute(ResourceInfo resource) {
		// 执行请求
		if (MethodEnum.GET.getValue().equals(resource.getResourceMethod())) {
			return get(resource);
		} else {
			String url = resource.getResourceUrl();
			int index = url.indexOf('?');
			if (index != -1) {
				resource.setResourceUrl(url.substring(0, index));
				resource.setParamMap(AggregatorUtils.getParamMap(url));
			}
			return post(resource);
		}
	}
	
	/**
     * 串行执行
     *
     * @param iter
     * @param value
     * @return
     */
    public String serialWorker(Iterator<ResourceInfo> iter, String value) {
        if (iter.hasNext()) {
            ResourceInfo resource = iter.next();
            if (StringUtils.isNotEmpty(value)) {
                // 使用提取参数模板提取上一个请求结果的参数
            	String content = beetlTemplate(resource.getResourceName(), value, resource.getParamTemplate());
            	if (StringUtils.isEmpty(content)) {
            		return value;
            	}
            	Map<String, String> paramMap = JsonUtils.string2Obj(content, new TypeReference<Map<String, String>>() {});
            	if (MapUtils.isEmpty(paramMap)) {
            		return value;
            	}
            	// 替换{xx}参数内容
            	resource.setResourceUrl(AggregatorUtils.replace(resource.getResourceUrl(), paramMap));
            }
            
            // 执行请求
            T t = execute(resource);
            
            // 获取执行结果
			String result = getResult(resource, t);
			
            // 递归调用
            return serialWorker(iter, result);
        }
        return value;
    }
	
	/**
	 * 获取执行结果
	 * 
	 * @param resource
	 * @param t
	 * @return
	 */
	private String getResult(ResourceInfo resource, T t) {
		// 获取响应结果
		String content = getResponse(resource, t);
		if (StringUtils.isEmpty(content)) {
			return resource.getDefaultValue();
		}

		// 根据模板获取内容
		content = beetlTemplate(resource.getResourceName(), content, resource.getResultTemplate());
		
		// 设置缓存
		setCache(resource, content);
		
		return StringUtils.isEmpty(content) ? resource.getDefaultValue() : content;
	}
	
	/**
	 * 获取响应结果
	 * 
	 * @param resource
	 * @param t
	 * @return
	 */
	private String getResponse(ResourceInfo resource, T t) {
		Transaction transaction = Cat.newTransaction(resource.getName(), resource.getOriginalUrl());
		try {
            String value = result(resource, t);
            // 成功
            transaction.setStatus(Message.SUCCESS);
            return value;
        } catch (Exception e) {
            // 失败
            transaction.setStatus(e);
            log.error("执行URL【{}】失败：", resource.getResourceUrl(), e);
            return null;
        } finally {
        	// 完成
        	transaction.complete();
		}
	}
	
	/**
	 * 设置缓存
	 *
	 * @param resource
	 * @param value
	 */
	private void setCache(ResourceInfo resource, String value) {
		if (!resource.getIsCache() || StringUtils.isEmpty(value)) {
			return;
		}
		// 缓存key
		String key = redisKey(resource);
		RBucket<String> rBucket = redisClient.getBucket(key);
		rBucket.set(value, resource.getCacheTime(), TimeUnit.MINUTES);
	}

	/**
	 * redis缓存key
	 * 
	 * @param resource
	 * @return
	 */
	private String redisKey(ResourceInfo resource) {
		return String.format(Constant.CACHEKEY, resource.getName(), resource.getResourceName(), resource.getResourceUrl().replace(":", ""));
	}

	/**
	 * 模板缓存
	 */
	private Map<Integer, Template> templateMap = new ConcurrentHashMap<>();

	/**
	 * 根据模板获取数据
	 * 
	 * @param key
	 * @param value
	 * @param templateStr
	 * @return
	 */
	public String beetlTemplate(String key, String value, String templateStr) {
		if (StringUtils.isEmpty(templateStr)) {
			return value;
		}
		Integer hashcode = templateStr.hashCode();
		Template template = templateMap.get(hashcode);
		if (null == template) {
			template = groupTemplate.getTemplate(templateStr);
			templateMap.put(hashcode, template);
		}
		template.binding(key, JsonUtils.string2JsonNode(value));
		return template.render();
	}

}
