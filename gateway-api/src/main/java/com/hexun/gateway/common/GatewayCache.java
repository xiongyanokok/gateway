package com.hexun.gateway.common;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.hexun.gateway.pojo.GatewayInfo;

/**
 * 网关配置信息
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午3:30:16
 */
public final class GatewayCache {
	
	private GatewayCache() {
		
	}

	private static final Map<String, GatewayInfo> MAP = new HashMap<>();
	
	public static void put(GatewayInfo gatewayInfo) {
		String key = "/" + gatewayInfo.getProjectInfo().getProjectName() + gatewayInfo.getUri();
		MAP.put(key, gatewayInfo);
	}
	
	public static GatewayInfo get(String key) {
		GatewayInfo apiGateway = null;
		for (Entry<String, GatewayInfo> entry : MAP.entrySet()) {
			if (key.startsWith(entry.getKey())) {
				apiGateway = entry.getValue();
				break;
			}
		}
		return apiGateway;
	}
}
