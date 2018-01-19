package com.hexun.gateway.disconf;

import com.baidu.disconf.client.common.annotations.DisconfFile;
import com.baidu.disconf.client.common.annotations.DisconfFileItem;

/**
 * 聚合配置信息
 * 
 * @author xiongyan
 * @date 2018年1月19日 上午11:41:43
 */
@DisconfFile(filename = "aggregator.properties")
public class AggregatorDisconf {
	
	private AggregatorDisconf() {
		
	}

	/**
	 * 聚合请求客户端
	 */
	private static String requestClient;

	@DisconfFileItem(name = "request.client")
	public static String getRequestClient() {
		return requestClient;
	}

	public static void setRequestClient(String requestClient) {
		AggregatorDisconf.requestClient = requestClient;
	}
	
}
