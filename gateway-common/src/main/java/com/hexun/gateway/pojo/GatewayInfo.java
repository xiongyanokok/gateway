package com.hexun.gateway.pojo;

import java.util.Set;

import lombok.Getter;
import lombok.Setter;

/**
 * 网关信息
 * 
 * @author xiongyan
 * @date 2017年12月25日 下午3:39:44
 */
@Getter
@Setter
public class GatewayInfo {

	/**
	 * 项目信息
	 */
	private ProjectInfo projectInfo;
	
	/**
	 * 接口地址
	 */
	private String uri;
	
	/**
	 * 是否启用
	 */
	private Boolean enabled;
	
	/**
	 * 方法：get/post
	 */
	private String method;
	
	/**
	 * 是否登录
	 */
	private Boolean login;
	
	/**
	 * 是否鉴权
	 */
	private Boolean sign;
	
	/**
	 * 鉴权信息
	 */
	private SignInfo signInfo;
	
	/**
	 * 是否缓存
	 */
	private Boolean cache;
	
	/**
	 * 缓存信息
	 */
	private CacheInfo cacheInfo;
	
	/**
	 * 是否加锁
	 */
	private Boolean lock;
	
	/**
	 * 是否限流
	 */
	private Boolean rateLimit;
	
	/**
	 * 限流信息
	 */
	private RateLimitInfo rateLimitInfo;
	
	/**
	 * 是否监控
	 */
	private Boolean monitor;
	
	/**
	 * 是否记录日志
	 */
	private Boolean log;
	
	/**
	 * ip黑名单
	 */
	private Set<String> ips;
	
	/**
	 * 用户黑名单
	 */
	private Set<Long> userIds;

}
