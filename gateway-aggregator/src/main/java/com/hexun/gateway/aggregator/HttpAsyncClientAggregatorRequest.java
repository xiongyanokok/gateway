package com.hexun.gateway.aggregator;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.apache.commons.collections4.MapUtils;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.nio.client.CloseableHttpAsyncClient;
import org.apache.http.impl.nio.client.HttpAsyncClients;
import org.apache.http.message.BasicNameValuePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;

import com.hexun.gateway.common.Constant;
import com.hexun.gateway.pojo.AggregationResource;

/**
 * HttpAsyncClient聚合请求
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:56:50
 */
@Component("httpAsyncClientAggregatorRequest")
public class HttpAsyncClientAggregatorRequest extends AbstractAggregatorRequest<Future<HttpResponse>> {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(HttpAsyncClientAggregatorRequest.class);
	
	/**
	 * HttpAsyncClients
	 */
	private CloseableHttpAsyncClient httpClient;
	
	/**
	 * 初始化 HttpAsyncClients
	 */
	@PostConstruct
	public void create() {
		httpClient = HttpAsyncClients.createDefault();
		httpClient.start();
	}
	
	/**
	 * 关闭 HttpAsyncClients
	 * 
	 * @throws IOException
	 */
	@PreDestroy
	public void close() throws IOException {
		if (null != httpClient) {
			httpClient.close();
		}
	}
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	public Future<HttpResponse> get(AggregationResource resource) {
		HttpGet request = new HttpGet(resource.getResourceUrl());
        if (resource.getIsLogin()) {
        	// 设置cookie
        	request.addHeader("Cookie", resource.getCookie());
        }
        return httpClient.execute(request, null);
	}
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	public Future<HttpResponse> post(AggregationResource resource) {
		HttpPost request = new HttpPost(resource.getResourceUrl());
        if (resource.getIsLogin()) {
        	// 设置cookie
        	request.addHeader("Cookie", resource.getCookie());
        }
        Map<String, String> paramMap = resource.getParamMap();
		if (MapUtils.isNotEmpty(paramMap)) {
			List<NameValuePair> nameValuePairs = new ArrayList<>();
			for (Map.Entry<String, String> entry : paramMap.entrySet()) {
				nameValuePairs.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
			}
			// 设置参数
			request.setEntity(new UrlEncodedFormEntity(nameValuePairs, Charset.forName(Constant.CHARSETNAME)));
		}
        return httpClient.execute(request, null);
	}
	
	/**
	 * 获取结果
	 * 
	 * @param resource
	 * @param future
	 * @return
	 */
	@Override
	public String result(AggregationResource resource, Future<HttpResponse> future) {
		try {
			HttpResponse response = future.get(resource.getTimeOut(), TimeUnit.SECONDS);
			if (HttpStatus.SC_OK == response.getStatusLine().getStatusCode()) {
				return StreamUtils.copyToString(response.getEntity().getContent(), Charset.forName(Constant.CHARSETNAME));
			}
			return null;
        } catch (Exception e) {
            logger.error("异步执行URL【{}】失败：", resource.getResourceUrl(), e);
            return null;
        }
	}
	
}
