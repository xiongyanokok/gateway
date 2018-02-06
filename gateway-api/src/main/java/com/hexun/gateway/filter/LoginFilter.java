package com.hexun.gateway.filter;

import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.Result;
import com.hexun.hwcommon.model.CommonLoginInfo;
import com.hexun.hwcommon.service.UserAuth;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

import lombok.extern.slf4j.Slf4j;

/**
 * 登录过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 上午10:56:04
 */
@Component
@Slf4j
public class LoginFilter extends ZuulFilter {
	
	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return GatewayUtils.isLogin();
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		CommonLoginInfo commonLoginInfo = UserAuth.GetUserInfoByRequest(RequestContext.getCurrentContext().getRequest());
		if (commonLoginInfo == null || Boolean.FALSE.toString().equalsIgnoreCase(commonLoginInfo.getIslogin())) {
			// 未登录
			GatewayUtils.responseBody(Result.NOTLOGIN.toString());
			log.info("用户未登录");
			return null;
		}
		
		// 用户id
		Long userId = Long.valueOf(commonLoginInfo.getUserid());
		// 用户黑名单
		Set<Long> userIds = GatewayUtils.getGatewayInfo().getUserIds();
		if (CollectionUtils.isNotEmpty(userIds) && userIds.contains(userId)) {
			GatewayUtils.responseBody(Result.NOTLOGIN.toString());
			log.info("用户ID【{}】已列入黑名单", userId);
			return null;
		}
			
		// 用户id放入threadLocal
		GatewayUtils.setUserId(userId);
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
		return 1;
	}

}
