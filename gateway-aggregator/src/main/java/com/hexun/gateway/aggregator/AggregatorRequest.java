package com.hexun.gateway.aggregator;

import java.util.List;

import com.hexun.gateway.pojo.AggregationResource;

/**
 * 聚合请求接口
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:55:24
 */
public interface AggregatorRequest {

	/**
	 * 并行
	 * 
	 * @param resources
	 * @return
	 */
	String parallel(List<AggregationResource> resources);
	
	/**
	 * 串行
	 * 
	 * @param resources
	 * @return
	 */
	String serial(List<AggregationResource> resources);
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	String get(AggregationResource resource);
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	String post(AggregationResource resource);
	
}
