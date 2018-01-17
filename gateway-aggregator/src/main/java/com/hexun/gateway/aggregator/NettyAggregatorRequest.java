package com.hexun.gateway.aggregator;

import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.hexun.gateway.pojo.AggregationResource;

import reactor.core.publisher.Mono;
import reactor.ipc.netty.http.client.HttpClient;
import reactor.ipc.netty.http.client.HttpClientResponse;

/**
 * netty实现聚合请求
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:56:00
 */
@Component("nettyAggregatorRequest")
public class NettyAggregatorRequest extends AbstractAggregatorRequest<Mono<HttpClientResponse>> {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(NettyAggregatorRequest.class);
	
	/**
	 * 并行
	 * 
	 * @param resources
	 * @return
	 */
	@Override
	public String parallel(List<AggregationResource> resources) {
		Map<String, String> resultMap = new HashMap<>();
		Map<AggregationResource, Mono<HttpClientResponse>> monoMap = new HashMap<>();
		for (AggregationResource resource : resources) {
			String value = getCacheResult(resource);
			if (StringUtils.isNotEmpty(value)) {
				resultMap.put(resource.getResourceName(), value);
			} else {
				monoMap.put(resource, execute(resource));
			}
		}

		for (Map.Entry<AggregationResource, Mono<HttpClientResponse>> entry : monoMap.entrySet()) {
			AggregationResource resource = entry.getKey();
			Mono<HttpClientResponse> mono = entry.getValue();
			// 获取执行结果
			String value = futureResult(resource, mono);
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
	@Override
	public Mono<HttpClientResponse> get(AggregationResource resource) {
		try {
			if (resource.getIsLogin()) {
				return HttpClient.create().get(resource.getResourceUrl(), req -> req.addHeader("Cookie", resource.getCookie()));
			} else {
				return HttpClient.create().get(resource.getResourceUrl());
			}
		} catch (Exception e) {
			logger.error("netty get error: ", e);
			return null;
		}
	}
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	@Override
	public Mono<HttpClientResponse> post(AggregationResource resource) {
		try {
			if (resource.getIsLogin()) {
				return HttpClient.create().post(resource.getResourceUrl(), req -> req.addHeader("Cookie", resource.getCookie()));
			} else {
				return HttpClient.create().post(resource.getResourceUrl(), null);
			}
		} catch (Exception e) {
			logger.error("netty get error: ", e);
			return null;
		}
	}
	
	/**
	 * 获取结果
	 * 
	 * @param resource
	 * @param mono
	 * @return
	 */
	@Override
	public String result(AggregationResource resource, Mono<HttpClientResponse> mono) {
		try {
			return mono.flatMapMany(s -> s.receive().asString()).reduce(String::concat).block(Duration.ofSeconds(resource.getTimeOut()));
		} catch (Exception e) {
			logger.error("异步执行URL【{}】失败：", resource.getResourceUrl(), e);
			return null;
		}
	}
	
}
