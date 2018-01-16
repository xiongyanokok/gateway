package com.hexun.gateway.filter;

import org.redisson.api.RLock;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;

import com.hexun.gateway.common.GatewayUtils;
import com.netflix.zuul.ZuulFilter;

/**
 * 释放锁过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午1:55:49
 */
public class UnLockFilter extends ZuulFilter {

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
		RLock lock = GatewayUtils.getLock();
		if (null != lock) {
			// 释放锁
			lock.unlock();
		}
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
		return FilterConstants.POST_TYPE;
	}

	/**
	 * 过滤器执行顺序
	 * 通过数字指定，数字越大，优先级越低
	 */
	@Override
	public int filterOrder() {
		return 998;
	}
	
}
