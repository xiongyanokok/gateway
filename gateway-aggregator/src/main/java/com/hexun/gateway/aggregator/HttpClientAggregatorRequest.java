package com.hexun.gateway.aggregator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.hexun.common.http.RequestPackage;
import com.hexun.common.http.ResponsePackage;
import com.hexun.gateway.common.ThreadPoolContext;
import com.hexun.gateway.pojo.AggregationResource;

/**
 * HttpClient聚合请求
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:56:50
 */
@Component("httpClientAggregatorRequest")
public class HttpClientAggregatorRequest extends AbstractAggregatorRequest {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(HttpClientAggregatorRequest.class);

	/**
	 * 并行
	 * 
	 * @param resources
	 * @return
	 */
	@Override
	public String parallel(List<AggregationResource> resources) {
		Map<AggregationResource, Future<String>> futureMap = new HashMap<>();
		for (AggregationResource resource : resources) {
			// 异步执行
			futureMap.put(resource, worker(resource));
		}

		Map<String, String> resultMap = new HashMap<>();
		for (Map.Entry<AggregationResource, Future<String>> entry : futureMap.entrySet()) {
			AggregationResource resource = entry.getKey();
			Future<String> future = entry.getValue();
			// 获取异步执行的结果
			String value = getAsyncResult(resource, future);
			if (StringUtils.isEmpty(value)) {
				value = resource.getDefaultValue();
			}
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
	 * 异步执行
	 * 
	 * @param resourceInfo
	 * @return
	 */
	private Future<String> worker(AggregationResource resource) {
    	return ThreadPoolContext.submit(() -> execute(resource));
    }
	
	/**
     * Future get
     * 
     * @param entry
     * @return
     */
    private String getAsyncResult(AggregationResource resource, Future<String> future) {
        try {
            return future.get(resource.getTimeOut(), TimeUnit.SECONDS);
        } catch (Exception e) {
            logger.error("并行异步执行URL【{}】失败：", resource.getResourceUrl(), e);
            return null;
        }
    }
    
	/**
	 * 串行
	 * 
	 * @param resources
	 * @return
	 */
	@Override
	public String serial(List<AggregationResource> resources) {
		return null;
	}
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	public String get(AggregationResource resource) {
		RequestPackage requestPackage = RequestPackage.get(resource.getResourceUrl());
        if (resource.getIsLogin()) {
        	// 设置cookie
            Map<String, String> cookieMap = new HashMap<>();
            cookieMap.put("Cookie", resource.getCookie());
            requestPackage.setHeaders(cookieMap);
        }

        // 执行http请求
        ResponsePackage response = requestPackage.setCharset("UTF-8").getResponse();
        if (null == response || !response.isSuccess()) {
            return null;
        }
       	return response.getContent();
	}
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	public String post(AggregationResource resource) {
		RequestPackage requestPackage = RequestPackage.post(resource.getResourceUrl());
        if (resource.getIsLogin()) {
        	// 设置cookie
            Map<String, String> cookieMap = new HashMap<>();
            cookieMap.put("Cookie", resource.getCookie());
            requestPackage.setHeaders(cookieMap);
        }
        Map<String, String> paramMap = resource.getParamMap();
        if (MapUtils.isNotEmpty(paramMap)) {
        	// 设置参数
        	requestPackage.setNameValuePairs(paramMap);
        }
        
        // 执行http请求
        ResponsePackage response = requestPackage.setCharset("UTF-8").getResponse();
        if (null == response || !response.isSuccess()) {
            return null;
        }
       	return response.getContent();
	}
	
}
