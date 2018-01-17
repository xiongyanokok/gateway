package com.hexun.gateway.aggregator;

import java.time.Duration;

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
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	@Override
	public Mono<HttpClientResponse> get(AggregationResource resource) {
		if (resource.getIsLogin()) {
			return HttpClient.create().get(resource.getResourceUrl(), req -> req.addHeader("Cookie", resource.getCookie()));
		} else {
			return HttpClient.create().get(resource.getResourceUrl());
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
		if (resource.getIsLogin()) {
			return HttpClient.create().post(resource.getResourceUrl(), req -> req.addHeader("Cookie", resource.getCookie()));
		} else {
			return HttpClient.create().post(resource.getResourceUrl(), null);
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
