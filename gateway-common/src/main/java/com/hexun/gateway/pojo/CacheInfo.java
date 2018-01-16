package com.hexun.gateway.pojo;

import java.util.concurrent.TimeUnit;

/**
 * 缓存信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午1:53:19
 */
public class CacheInfo {

	/**
	 * 缓存时长
	 */
	private Long time;
	
	/**
	 * 时间单位
	 */
	private TimeUnit timeUnit;

	public Long getTime() {
		return time;
	}

	public void setTime(Long time) {
		this.time = time;
	}

	public TimeUnit getTimeUnit() {
		return timeUnit;
	}

	public void setTimeUnit(TimeUnit timeUnit) {
		this.timeUnit = timeUnit;
	}
}
