package com.hexun.gateway.pojo;

/**
 * 限流信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:24
 */
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

	public Long getLimit() {
		return limit;
	}

	public void setLimit(Long limit) {
		this.limit = limit;
	}

	public Long getQuota() {
		return quota;
	}

	public void setQuota(Long quota) {
		this.quota = quota;
	}

	public Long getRefreshInterval() {
		return refreshInterval;
	}

	public void setRefreshInterval(Long refreshInterval) {
		this.refreshInterval = refreshInterval;
	}

}
