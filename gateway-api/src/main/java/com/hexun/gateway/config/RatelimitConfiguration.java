package com.hexun.gateway.config;

import static java.util.concurrent.TimeUnit.SECONDS;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.netflix.zuul.filters.Route;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.hexun.cache.RedisMixCacheManager;
import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.RateLimitInfo;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.DefaultRateLimitKeyGenerator;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.Rate;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.RateLimitKeyGenerator;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.RateLimiter;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.properties.RateLimitProperties;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.properties.RateLimitProperties.Policy;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.repository.RateLimiterErrorHandler;
import com.marcosbarbero.cloud.autoconfigure.zuul.ratelimit.config.repository.RedisRateLimiter;

/**
 * 限流
 * 
 * @author xiongyan
 * @date 2017年12月21日 下午5:46:40
 */
@Configuration
public class RatelimitConfiguration {
	
	@Autowired
    private RedisMixCacheManager cacheManager;
	
	private static final String QUOTA_SUFFIX = "-quota";
	

	@Bean
	public RateLimitKeyGenerator rateLimitKeyGenerator(final RateLimitProperties properties) {
		return new DefaultRateLimitKeyGenerator(properties) {
			@Override
			public String key(HttpServletRequest request, Route route, Policy policy) {
				if (GatewayUtils.isRateLimit()) {
					// 获取限流信息
					RateLimitInfo rateLimitInfo = GatewayUtils.getGatewayInfo().getRateLimitInfo();
					policy.setLimit(rateLimitInfo.getLimit());
					policy.setQuota(rateLimitInfo.getQuota());
					policy.setRefreshInterval(rateLimitInfo.getRefreshInterval());
				}
				return super.key(request, route, policy);
			}
		};
	}
	
	
	@Bean
	public RateLimiter rateLimiter(final RateLimiterErrorHandler rateLimiterErrorHandler) {
		return new RedisRateLimiter(rateLimiterErrorHandler, null) {
			@Override
		    public Rate consume(final Policy policy, final String key, final Long requestTime) {
		        final Long refreshInterval = policy.getRefreshInterval();
		        final Long quota = policy.getQuota() != null ? SECONDS.toMillis(policy.getQuota()) : null;
		        final Rate rate = new Rate(key, policy.getLimit(), quota, null, null);

		        // request number limit per refresh interval window
		        calcRemainingLimit(policy.getLimit(), refreshInterval, requestTime, key, rate);
		        
		        // request time limit per refresh interval window (in seconds)
		        calcRemainingQuota(quota, refreshInterval, requestTime, key, rate);

		        return rate;
		    }

		    private void calcRemainingLimit(Long limit, Long refreshInterval, Long requestTime, String key, Rate rate) {
		        if (limit != null) {
		            handleExpiration(key, refreshInterval, rate);
		            long usage = requestTime == null ? 1L : 0L;
		            Long current = 0L;
		            try {
		            	current = cacheManager.getRedissonClient().getAtomicLong(key).addAndGet(usage);
		            } catch (RuntimeException e) {
		                String msg = "Failed retrieving rate for " + key + ", will return limit";
		                rateLimiterErrorHandler.handleError(msg, e);
		            }
		            rate.setRemaining(Math.max(-1, limit - current));
		        }
		    }

		    private void calcRemainingQuota(Long quota, Long refreshInterval, Long requestTime, String key, Rate rate) {
		        if (quota != null) {
		            String quotaKey = key + QUOTA_SUFFIX;
		            handleExpiration(quotaKey, refreshInterval, rate);
		            long usage = requestTime != null ? requestTime : 0L;
		            Long current = 0L;
		            try {
		                current = cacheManager.getRedissonClient().getAtomicLong(quotaKey).addAndGet(usage);
		            } catch (RuntimeException e) {
		                String msg = "Failed retrieving rate for " + quotaKey + ", will return quota limit";
		                rateLimiterErrorHandler.handleError(msg, e);
		            }
		            rate.setRemainingQuota(Math.max(-1, quota - current));
		        }
		    }

		    private void handleExpiration(String key, Long refreshInterval, Rate rate) {
		        Long expire = null;
		        try {
		            expire = cacheManager.getRedissonClient().getAtomicLong(key).remainTimeToLive();
		            if (expire == null || expire == -1) {
		                cacheManager.getRedissonClient().getAtomicLong(key).expire(refreshInterval, SECONDS);
			            expire = refreshInterval;
		            }
		        } catch (RuntimeException e) {
		            String msg = "Failed retrieving expiration for " + key + ", will reset now";
		            rateLimiterErrorHandler.handleError(msg, e);
		        }
		        rate.setReset(SECONDS.toMillis(expire == null ? 0L : expire));
		    }
		};
	}
	
}
