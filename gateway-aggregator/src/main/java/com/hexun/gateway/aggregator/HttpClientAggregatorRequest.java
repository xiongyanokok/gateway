package com.hexun.gateway.aggregator;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import com.hexun.common.http.RequestPackage;
import com.hexun.common.http.ResponsePackage;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.pojo.ResourceInfo;

/**
 * HttpClient聚合请求
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:56:50
 */
@Component("httpClientAggregatorRequest")
public class HttpClientAggregatorRequest extends AbstractAggregatorRequest<Future<String>> {
	
	@Autowired
	private ThreadPoolTaskExecutor taskExecutor;
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	@Override
	public Future<String> get(ResourceInfo resource) {
		return taskExecutor.submit(() -> {
			RequestPackage requestPackage = RequestPackage.get(resource.getResourceUrl());
	        if (resource.getIsLogin()) {
	        	// 设置cookie
	            Map<String, String> cookieMap = new HashMap<>(1);
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
	public Future<String> post(ResourceInfo resource) {
		return taskExecutor.submit(() -> {
			RequestPackage requestPackage = RequestPackage.post(resource.getResourceUrl());
			if (resource.getIsLogin()) {
				// 设置cookie
				Map<String, String> cookieMap = new HashMap<>(1);
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
	 * @throws Exception
	 */
	@Override
	public String result(ResourceInfo resource, Future<String> future) throws Exception {
		return future.get(resource.getTimeOut(), TimeUnit.SECONDS);
	}
	
}
