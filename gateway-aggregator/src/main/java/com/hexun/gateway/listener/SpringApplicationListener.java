package com.hexun.gateway.listener;

import javax.annotation.PostConstruct;

import org.apache.curator.framework.recipes.cache.TreeCache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.hexun.gateway.zookeeper.RegistryCenter;

/**
 * SpringApplicationListener
 * 
 * @author xiongyan
 * @date 2018年1月9日 下午3:00:21
 */
@Component
public class SpringApplicationListener {

	@Autowired
	private RegistryCenter registryCenter; 
	
	/**
	 * 初始化聚合配置信息
	 */
	@PostConstruct
	public void init() {
		ZookeeperEventListener listener = new ZookeeperEventListener();
		// 获取聚合配置信息
		listener.reloadAggregationInfo();
		
		// 聚合配置信息发生变化通知客户端重新加载数据
		String basePath = "/";
		// 对根节点下所有孩子节点的监听
		registryCenter.addCacheData(basePath);
		TreeCache cache = registryCenter.getCache(basePath);
        cache.getListenable().addListener(listener);
	}
	
}
