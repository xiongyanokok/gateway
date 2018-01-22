package com.hexun.gateway.disconf;

import com.baidu.disconf.client.common.annotations.DisconfFile;
import com.baidu.disconf.client.common.annotations.DisconfFileItem;

/**
 * 配置信息
 * 
 * @author xiongyan
 * @date 2018年1月19日 上午11:41:43
 */
@DisconfFile(filename = "common.properties")
public class CommonDisconf {
	
	private CommonDisconf() {
		
	}

	/**
	 * 聚合请求客户端
	 */
	private static String requestClient;
	
	/**
	 * 聚合配置数据地址
	 */
	private static String aggregatorUrl;
	
	/**
	 * 网关配置数据地址
	 */
	private static String gatewayUrl;

	@DisconfFileItem(name = "request.client")
	public static String getRequestClient() {
		return requestClient;
	}

	public static void setRequestClient(String requestClient) {
		CommonDisconf.requestClient = requestClient;
	}

	@DisconfFileItem(name = "aggregator.url")
	public static String getAggregatorUrl() {
		return aggregatorUrl;
	}

	public static void setAggregatorUrl(String aggregatorUrl) {
		CommonDisconf.aggregatorUrl = aggregatorUrl;
	}

	@DisconfFileItem(name = "gateway.url")
	public static String getGatewayUrl() {
		return gatewayUrl;
	}

	public static void setGatewayUrl(String gatewayUrl) {
		CommonDisconf.gatewayUrl = gatewayUrl;
	}
	
}
