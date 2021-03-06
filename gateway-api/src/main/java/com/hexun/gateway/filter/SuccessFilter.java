package com.hexun.gateway.filter;

import java.io.IOException;
import java.nio.charset.Charset;

import org.redisson.api.RBucket;
import org.redisson.api.RLock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StreamUtils;

import com.dianping.cat.message.Transaction;
import com.hexun.cache.IRedisClient;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.CacheInfo;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

/**
 * 请求成功过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午1:55:49
 */
@Component
public class SuccessFilter extends ZuulFilter {
	
	@Autowired
    private IRedisClient redisClient;
	
	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		if (GatewayUtils.isEnd()) {
			return false;
		}
		RequestContext ctx = RequestContext.getCurrentContext();
		return null == ctx.getThrowable() && (null != ctx.getResponseDataStream() || null != ctx.getResponseBody());
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		try {
			// 释放锁
			if (GatewayUtils.isLock()) {
				RLock lock = GatewayUtils.getLock();
				if (null != lock) {
					lock.unlock();
				}
			}
			
			// 分布式缓存
			if (GatewayUtils.isCache()) {
				RequestContext ctx = RequestContext.getCurrentContext();
				// 获取流中的内容
				String body = StreamUtils.copyToString(ctx.getResponseDataStream(), Charset.forName(Constant.CHARSETNAME));
				// 字符串放入responseBody
				ctx.setResponseBody(body);
				
				// 生成唯一key
				String key = String.format(Constant.CACHEKEY, GatewayUtils.getProjectName(), ctx.getRequest().getRequestURI().replace(":", ""));
				RBucket<String> rBucket = redisClient.getBucket(key);
				CacheInfo cacheInfo = GatewayUtils.getGatewayInfo().getCacheInfo();
				rBucket.set(body, cacheInfo.getTime(), cacheInfo.getTimeUnit());
			}
			
			// 获取CAT Transaction
			Transaction transaction = GatewayUtils.getTransaction();
			if (null != transaction) {
				transaction.setStatus(Transaction.SUCCESS);
				transaction.complete();
			}
		} catch (IOException e) {
			ReflectionUtils.rethrowRuntimeException(e);
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
		return 999;
	}

}
