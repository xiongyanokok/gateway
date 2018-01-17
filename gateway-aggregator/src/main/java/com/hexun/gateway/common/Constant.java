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
	 * 内容类型
	 */
	public static final String CONTENTTYPE = "Content-Type";
	
	/**
	 * 媒体类型
	 */
	public static final String MEDIATYPE = "application/json; charset=UTF-8";
	
	/**
	 * jsonp
	 */
	public static final String CALLBACK = "callback";
	
	/**
	 * 前缀
	 */
	public static final String PREFIX = "/apigw/";
	
	/**
	 * 分布式缓存key
	 */
	public static final String CACHEKEY = "aggregator:%s:%s:%s";
	
	/**
	 * 登录地址
	 */
	public static final String LOGINURL = "http://reg.intcoop.hexun.com/wapreg/checklogin.aspx?format=json";
}
