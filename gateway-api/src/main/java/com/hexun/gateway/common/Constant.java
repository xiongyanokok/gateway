package com.hexun.gateway.common;

/**
 * 常量
 * 
 * @author xiongyan
 * @date 2017年12月21日 上午11:54:31
 */
public final class Constant extends BaseConstant {
	
	/**
	 * route
	 */
	public static final String ROUTE = "route";
	
	/**
	 * 分布式缓存key
	 */
	public static final String CACHEKEY = "gateway:%s:cache:%s";
	
	/**
	 * 分布式锁key
	 */
	public static final String LOCKKEY = "gateway:%s:lock:%s";
	
}
