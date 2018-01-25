package com.hexun.gateway.pojo;

import java.util.concurrent.TimeUnit;

import lombok.Getter;
import lombok.Setter;

/**
 * 缓存信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午1:53:19
 */
@Getter
@Setter
public class CacheInfo {

	/**
	 * 缓存时长
	 */
	private Long time;
	
	/**
	 * 时间单位
	 */
	private TimeUnit timeUnit;

}
