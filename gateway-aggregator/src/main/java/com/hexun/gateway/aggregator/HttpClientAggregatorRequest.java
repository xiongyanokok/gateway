package com.hexun.gateway.aggregator;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.MapUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.hexun.common.http.RequestPackage;
import com.hexun.common.http.ResponsePackage;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.common.ThreadPoolContext;
import com.hexun.gateway.pojo.AggregationResource;

/**
 * HttpClient聚合请求
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:56:50
 */
@Component("httpClientAggregatorRequest")
public class HttpClientAggregatorRequest extends AbstractAggregatorRequest<Future<String>> {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(HttpClientAggregatorRequest.class);
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	@Override
	public Future<String> get(AggregationResource resource) {
		return ThreadPoolContext.submit(() -> {
			RequestPackage requestPackage = RequestPackage.get(resource.getResourceUrl());
	        if (resource.getIsLogin()) {
	        	// 设置cookie
	            Map<String, String> cookieMap = new HashMap<>();
	            cookieMap.put("Cookie", resource.getCookie());
	            requestPackage.setHeaders(cookieMap);
	        }
	
	        // 执行http请求
	        ResponsePackage response = requestPackage.setCharset(Constant.CHARSETNAME).getResponse();
	        if (null == response || !response.isSuccess()) {
	            return null;
	        }
	       	return response.getContent();
		});
	}
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	@Override
	public Future<String> post(AggregationResource resource) {
		return ThreadPoolContext.submit(() -> {
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
			ResponsePackage response = requestPackage.setCharset(Constant.CHARSETNAME).getResponse();
			if (null == response || !response.isSuccess()) {
				return null;
			}
			return response.getContent();
		});
	}
	
	/**
	 * 获取结果
	 * 
	 * @param resource
	 * @param future
	 * @return
	 */
	@Override
	public String result(AggregationResource resource, Future<String> future) {
		try {
            return future.get(resource.getTimeOut(), TimeUnit.SECONDS);
        } catch (Exception e) {
            logger.error("异步执行URL【{}】失败：", resource.getResourceUrl(), e);
            return null;
        }
	}
	
}
