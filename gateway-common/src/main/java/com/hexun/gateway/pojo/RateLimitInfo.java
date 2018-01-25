package com.hexun.gateway.pojo;

import lombok.Getter;
import lombok.Setter;

/**
 * 限流信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:24
 */
@Getter
@Setter
public class RateLimitInfo {

	/**
     * 请求数量限制
     */
	private Long limit;
	
	/**
     * 请求时间限制(秒)
     */
	private Long quota;
	
	/**
     * 刷新间隔(秒)
     */
	private Long refreshInterval;

}
