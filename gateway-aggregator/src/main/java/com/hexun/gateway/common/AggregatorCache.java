package com.hexun.gateway.common;

import java.util.HashMap;
import java.util.Map;

import com.hexun.gateway.pojo.AggregatorInfo;

/**
 * 聚合配置信息
 * 
 * @author xiongyan
 * @date 2018年1月17日 下午3:09:12
 */
public final class AggregatorCache {

	private AggregatorCache() {
		
	}

	private static final Map<String, AggregatorInfo> MAP = new HashMap<>();
	
	public static void put(AggregatorInfo aggregatorInfo) {
		MAP.put(aggregatorInfo.getName(), aggregatorInfo);
	}
	
	public static AggregatorInfo get(String key) {
		return MAP.get(key);
	}
	
}
