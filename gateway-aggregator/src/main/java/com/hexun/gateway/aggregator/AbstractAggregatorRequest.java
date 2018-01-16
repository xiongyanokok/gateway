package com.hexun.gateway.aggregator;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.beetl.core.Configuration;
import org.beetl.core.GroupTemplate;
import org.beetl.core.Template;
import org.beetl.core.resource.StringTemplateResourceLoader;
import org.redisson.api.RBucket;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.hexun.cache.IRedisClient;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.pojo.AggregationResource;

/**
 * 聚合请求抽象类
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:55:24
 */
public abstract class AbstractAggregatorRequest implements AggregatorRequest {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(AbstractAggregatorRequest.class);
	
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
	 * 模板缓存
	 */
	private Map<Integer, Template> templateMap = new ConcurrentHashMap<>();
	
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
	 * 
	 * @param resource
	 * @return
	 */
	public String execute(AggregationResource resource) {
		String url = resource.getResourceUrl();
		if (StringUtils.isEmpty(url)) {
			return null;
		}

		// 获取缓存
		String content = getCache(resource);
		if (StringUtils.isNotEmpty(content)) {
			return content;
		}

		// 执行请求
		if ("GET".equalsIgnoreCase(resource.getResourceMethod())) {
			content = get(resource);
		} else {
			int index = url.indexOf('?');
			if (index != -1) {
				resource.setResourceUrl(url.substring(0, index));
				Map<String, String> paramMap = postData(url.substring(index + 1));
				resource.setParamMap(paramMap);
			}
			content = post(resource);
		}
		if (StringUtils.isEmpty(content)) {
			return content;
		}

		// 根据模板获取内容
		content = beetlTemplate(resource.getResourceName(), content, resource.getResourceTemplate());
		
		// 设置缓存
		setCache(resource, content);
		
		return content;
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
	 * 获取缓存
	 *
	 * @param resource
	 * @return
	 */
	private String getCache(AggregationResource resource) {
		if (!resource.getIsCache()) {
			return null;
		}
		// 缓存key
		String key = redisKey(resource);
		RBucket<String> rBucket = redisClient.getBucket(key);
		return rBucket.get();
	}

	/**
	 * redis缓存key
	 * 
	 * @param resource
	 * @return
	 */
	private String redisKey(AggregationResource resource) {
		return String.format("cdsq:polymeric:%s:%s:%s", resource.getName(), resource.getResourceName(), resource.getResourceUrl().replace(":", ""));
	}

	/**
	 * 参数转map
	 *
	 * @param param
	 * @return
	 */
	private Map<String, String> postData(String param) {
		if (StringUtils.isEmpty(param)) {
			return null;
		}
		Map<String, String> map = new HashMap<>();
		String[] params = param.split("&");
		for (String str : params) {
			String[] strs = str.split("=");
			if (null != strs && strs.length == 2) {
				map.put(strs[0], strs[1]);
			}
		}
		return map;
	}

	/**
	 * 根据模板获取数据
	 * 
	 * @param key
	 * @param value
	 * @param templateStr
	 * @return
	 */
	private String beetlTemplate(String key, String value, String templateStr) {
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
