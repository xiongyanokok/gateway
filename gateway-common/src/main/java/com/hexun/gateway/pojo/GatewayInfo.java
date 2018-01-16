package com.hexun.gateway.pojo;

import java.util.Set;

/**
 * 网关信息
 * 
 * @author xiongyan
 * @date 2017年12月25日 下午3:39:44
 */
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

	public ProjectInfo getProjectInfo() {
		return projectInfo;
	}

	public void setProjectInfo(ProjectInfo projectInfo) {
		this.projectInfo = projectInfo;
	}

	public String getUri() {
		return uri;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public Boolean getEnabled() {
		return enabled;
	}

	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}

	public String getMethod() {
		return method;
	}

	public void setMethod(String method) {
		this.method = method;
	}

	public Boolean getLogin() {
		return login;
	}

	public void setLogin(Boolean login) {
		this.login = login;
	}

	public Boolean getSign() {
		return sign;
	}

	public void setSign(Boolean sign) {
		this.sign = sign;
	}

	public SignInfo getSignInfo() {
		return signInfo;
	}

	public void setSignInfo(SignInfo signInfo) {
		this.signInfo = signInfo;
	}

	public Boolean getCache() {
		return cache;
	}

	public void setCache(Boolean cache) {
		this.cache = cache;
	}

	public CacheInfo getCacheInfo() {
		return cacheInfo;
	}

	public void setCacheInfo(CacheInfo cacheInfo) {
		this.cacheInfo = cacheInfo;
	}

	public Boolean getLock() {
		return lock;
	}

	public void setLock(Boolean lock) {
		this.lock = lock;
	}

	public Boolean getRateLimit() {
		return rateLimit;
	}

	public void setRateLimit(Boolean rateLimit) {
		this.rateLimit = rateLimit;
	}

	public RateLimitInfo getRateLimitInfo() {
		return rateLimitInfo;
	}

	public void setRateLimitInfo(RateLimitInfo rateLimitInfo) {
		this.rateLimitInfo = rateLimitInfo;
	}

	public Boolean getMonitor() {
		return monitor;
	}

	public void setMonitor(Boolean monitor) {
		this.monitor = monitor;
	}

	public Boolean getLog() {
		return log;
	}

	public void setLog(Boolean log) {
		this.log = log;
	}

	public Set<String> getIps() {
		return ips;
	}

	public void setIps(Set<String> ips) {
		this.ips = ips;
	}

	public Set<Long> getUserIds() {
		return userIds;
	}

	public void setUserIds(Set<Long> userIds) {
		this.userIds = userIds;
	}

}
