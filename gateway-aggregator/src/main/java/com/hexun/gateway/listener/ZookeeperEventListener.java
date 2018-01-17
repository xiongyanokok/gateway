package com.hexun.gateway.listener;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.hexun.common.http.RequestPackage;
import com.hexun.common.http.ResponsePackage;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.common.AggregationCache;
import com.hexun.gateway.enums.ErrorCodeEnum;
import com.hexun.gateway.exception.GatewayException;
import com.hexun.gateway.pojo.AggregationInfo;

/**
 * zookeeper 事件监听
 * 
 * @author xiongyan
 * @date 2017年4月21日 上午11:15:53
 */
public class ZookeeperEventListener implements TreeCacheListener {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ZookeeperEventListener	.class);
	
	/**
	 * 重试次数
	 */
	private int retryTimes = 3;
	
	/**
	 * 休眠时间
	 */
	private int retryInterval = 5000; 
	
	/**
	 * 事件
	 * 
	 * @param client
	 * @param event
	 */
	@Override
	public void childEvent(CuratorFramework client, TreeCacheEvent event) throws Exception {
		if (TreeCacheEvent.Type.NODE_UPDATED.equals(event.getType())) {
			String data = new String(event.getData().getData());
			logger.info("聚合配置信息发生变化，时间【{}】", data);
			
			// 加载聚合配置信息
			reloadAggregationInfo();
			logger.info("聚合配置信息重新加载成功");
		}
	}
	
	/**
	 * 加载聚合配置信息
	 */
	public void reloadAggregationInfo() {
		// 获取聚合配置信息
		String result = getAggregationInfo();
		if (StringUtils.isEmpty(result)) {
			logger.error("获取聚合配置信息为空");
			return;
		}
		// 序列化
		List<AggregationInfo> aggregationInfos = JsonUtils.string2Obj(result, new TypeReference<List<AggregationInfo>>() { });
		if (CollectionUtils.isEmpty(aggregationInfos)) {
			logger.error("聚合配置信息【{}】序列化失败", result);
			return;
		}
		// 放入内存
		for (AggregationInfo aggregationInfo : aggregationInfos) {
			AggregationCache.put(aggregationInfo);
		}
	}
	
	/**
	 * 获取聚合配置信息
	 * 
	 * @return
	 */
	private String getAggregationInfo() {
        int times = 0;
        while (times < retryTimes) {
        	/*ResponsePackage response = RequestPackage.get("http://localhost:8080/gateway").getResponse();
			if (null == response || !response.isSuccess()) {
				times++;
				sleep();
			} else {
				return response.getContent();
			}*/
        	return JsonUtils.obj2String(AggregationCache.init());
        }
        logger.error("重试【{}】次依然失败", retryTimes);
        throw new GatewayException(ErrorCodeEnum.RETRY_TIMES, "重试【"+retryTimes+"】次");
    }
	
	private void sleep() {
        try {
            Thread.sleep(retryInterval);
        } catch (Exception e) {
        	// 
        }
    }
	
}