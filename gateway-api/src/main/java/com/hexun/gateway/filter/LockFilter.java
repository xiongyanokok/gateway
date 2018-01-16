package com.hexun.gateway.filter;

import java.util.concurrent.TimeUnit;

import org.redisson.api.RLock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;

import com.hexun.cache.IRedisClient;
import com.hexun.common.security.Md5Utils;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.common.GatewayUtils;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

/**
 * 分布式锁过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午1:55:49
 */
public class LockFilter extends ZuulFilter {
	
	@Autowired
    private IRedisClient redisClient;

	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return GatewayUtils.isLock();
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		String uri = RequestContext.getCurrentContext().getRequest().getRequestURI();
		// 生成唯一key
		String key = String.format(Constant.LOCKKEY, GatewayUtils.getProjectName(), Md5Utils.md5(uri + "_" + GatewayUtils.getUserId()));
		// 获取锁
		RLock lock = redisClient.getLock(key);
		// 过期解锁，5秒钟后自动解锁
		lock.lock(5, TimeUnit.SECONDS);
		// RLock放入threadLocal
		GatewayUtils.setLock(lock);
		return null;
	}

	/**
	 * 过滤器的类型
	 * pre：可以在请求被路由之前调用
	 * route：在路由请求时候被调用
	 * post：在route和error过滤器之后被调用
	 * error：处理请求时发生错误时被调用
	 */
	@Override
	public String filterType() {
		return FilterConstants.PRE_TYPE;
	}

	/**
	 * 过滤器执行顺序
	 * 通过数字指定，数字越大，优先级越低
	 */
	@Override
	public int filterOrder() {
		return 4;
	}
	
}
