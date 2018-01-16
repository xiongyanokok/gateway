package com.hexun.gateway.common;

/**
 * 常量
 * 
 * @author xiongyan
 * @date 2017年12月21日 上午11:54:31
 */
public final class Constant {
	
	private Constant() {
		
	}
	
	/**
	 * 编码
	 */
	public static final String CHARSETNAME = "UTF-8";
	
	/**
	 * 类型
	 */
	public static final String CONTENTTYPE = "Content-Type";
	
	/**
	 * jsonp
	 */
	public static final String CALLBACK = "callback";
	
	/**
	 * route
	 */
	public static final String ROUTE = "route";
	
	/**
	 * 分布式缓存key
	 */
	public static final String CACHEKEY = "zuul:%s:cache:%s";
	
	/**
	 * 分布式锁key
	 */
	public static final String LOCKKEY = "zuul:%s:lock:%s";
	
}
