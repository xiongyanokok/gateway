package com.hexun.gateway.filter;

import org.redisson.api.RBucket;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.hexun.cache.IRedisClient;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.common.GatewayUtils;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

/**
 * 分布式缓存过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午1:55:49
 */
@Component
public class CacheFilter extends ZuulFilter {
	
	@Autowired
    private IRedisClient redisClient;

	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return GatewayUtils.isCache();
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		String uri = RequestContext.getCurrentContext().getRequest().getRequestURI();
		// 生成唯一key
		String key = String.format(Constant.CACHEKEY, GatewayUtils.getProjectName(), uri.replace(":", ""));
		RBucket<String> rBucket = redisClient.getBucket(key);
		if (rBucket.isExists()) {
			GatewayUtils.responseBody(rBucket.get());
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
		return FilterConstants.PRE_TYPE;
	}

	/**
	 * 过滤器执行顺序
	 * 通过数字指定，数字越大，优先级越低
	 */
	@Override
	public int filterOrder() {
		return 5;
	}

}
