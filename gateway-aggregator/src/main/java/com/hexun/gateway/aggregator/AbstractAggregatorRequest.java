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

import com.fasterxml.jackson.core.type.TypeReference;
import com.hexun.cache.IRedisClient;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.common.AggregationUtils;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.pojo.AggregationResource;

import io.netty.handler.codec.http.HttpMethod;

/**
 * 聚合请求抽象类
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:55:24
 */
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
     * JsonUtils
     */
    public static final JsonUtils JSONUTILS = new JsonUtils();

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
	public String parallel(List<AggregationResource> resources) {
		Map<String, String> resultMap = new HashMap<>();
		Map<AggregationResource, T> futureMap = new HashMap<>();
		for (AggregationResource resource : resources) {
			String value = getCacheResult(resource);
			if (StringUtils.isNotEmpty(value)) {
				resultMap.put(resource.getResourceName(), value);
			} else {
				futureMap.put(resource, execute(resource));
			}
		}

		for (Map.Entry<AggregationResource, T> entry : futureMap.entrySet()) {
			AggregationResource resource = entry.getKey();
			T t = entry.getValue();
			// 获取执行结果
			String value = futureResult(resource, t);
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
	public String serial(List<AggregationResource> resources) {
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
	public String getCacheResult(AggregationResource resource) {
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
	public T execute(AggregationResource resource) {
		// 执行请求
		if (HttpMethod.GET.name().equalsIgnoreCase(resource.getResourceMethod())) {
			return get(resource);
		} else {
			String url = resource.getResourceUrl();
			int index = url.indexOf('?');
			if (index != -1) {
				resource.setResourceUrl(url.substring(0, index));
				resource.setParamMap(AggregationUtils.getParamMap(url));
			}
			return post(resource);
		}
	}
	
	/**
     * 串行执行
     *
     * @param iter
     * @param value
     */
    public String serialWorker(Iterator<AggregationResource> iter, String value) {
        if (iter.hasNext()) {
            AggregationResource resource = iter.next();
            if (StringUtils.isNotEmpty(value)) {
                // 使用提取参数模板提取上一个请求结果的参数
            	String content = beetlTemplate(resource.getResourceName(), value, resource.getResourceRegex());
            	if (StringUtils.isEmpty(content)) {
            		return value;
            	}
            	Map<String, String> paramMap = JSONUTILS.deserialize(content, new TypeReference<Map<String, String>>() {});
            	if (MapUtils.isEmpty(paramMap)) {
            		return value;
            	}
            	// 替换{xx}参数内容
            	resource.setResourceUrl(AggregationUtils.replace(resource.getResourceUrl(), paramMap));
            }
            
            // 执行请求
            T t = execute(resource);
            
            // 获取执行结果
			String result = futureResult(resource, t);
			
            // 递归调用
            return serialWorker(iter, result);
        }
        return value;
    }
	
	/**
	 * 获取异步结果
	 * 
	 * @param resource
	 * @param t
	 * @return
	 */
	public String futureResult(AggregationResource resource, T t) {
		// 获取结果
		String content = result(resource, t);
		if (StringUtils.isEmpty(content)) {
			return resource.getDefaultValue();
		}

		// 根据模板获取内容
		content = beetlTemplate(resource.getResourceName(), content, resource.getResourceTemplate());
		
		// 设置缓存
		setCache(resource, content);
		
		return StringUtils.isEmpty(content) ? resource.getDefaultValue() : content;
	}
	
	/**
	 * 设置缓存
	 *
	 * @param resource
	 * @param value
	 */
	private void setCache(AggregationResource resource, String value) {
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
	private String redisKey(AggregationResource resource) {
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
