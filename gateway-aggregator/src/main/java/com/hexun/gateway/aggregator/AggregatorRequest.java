package com.hexun.gateway.aggregator;

import java.util.List;

import com.hexun.gateway.pojo.ResourceInfo;

/**
 * 聚合请求接口
 * 
 * @author xiongyan
 * @date 2018年1月15日 下午1:55:24
 */
public interface AggregatorRequest<T> {

	/**
	 * 并行
	 * 
	 * @param resources
	 * @return
	 */
	String parallel(List<ResourceInfo> resources);
	
	/**
	 * 串行
	 * 
	 * @param resources
	 * @return
	 */
	String serial(List<ResourceInfo> resources);
	
	/**
	 * 执行get请求
	 * 
	 * @param resource
	 * @return
	 */
	T get(ResourceInfo resource);
	
	/**
	 * 执行post请求
	 * 
	 * @param resource
	 * @return
	 */
	T post(ResourceInfo resource);
	
	/**
	 * 获取结果
	 * 
	 * @param resource
	 * @param t
	 * @return
	 * @throws Exception
	 */
	String result(ResourceInfo resource, T t) throws Exception;
	
}
