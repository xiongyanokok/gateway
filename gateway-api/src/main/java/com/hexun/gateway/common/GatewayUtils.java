package com.hexun.gateway.common;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RLock;
import org.springframework.cloud.netflix.zuul.filters.Route;
import org.springframework.cloud.netflix.zuul.filters.RouteLocator;
import org.springframework.http.MediaType;
import org.springframework.web.util.UrlPathHelper;

import com.hexun.gateway.config.SpringContextUtils;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.pojo.GatewayInfo;
import com.netflix.zuul.context.RequestContext;

/**
 * GatewayUtils
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午3:30:16
 */
public class GatewayUtils {
	
	private GatewayUtils() {
		
	}
	
	/**
	 * UrlPathHelper
	 */
	private static final UrlPathHelper URLPATHHELPER = new UrlPathHelper();
	
	/**
	 * RouteLocator
	 */
	private static RouteLocator routeLocator = SpringContextUtils.getBean(RouteLocator.class);

	/**
	 * 是否终止
	 * 
	 * @return
	 */
	public static boolean isEnd() {
		return !RequestContext.getCurrentContext().sendZuulResponse();
	}
	
	/**
	 * 获取Route
	 * 
	 * @return
	 */
	public static Route getRoute() {
		RequestContext ctx = RequestContext.getCurrentContext();
		Route route = (Route) ctx.get(Constant.ROUTE); 
		if (null == route) {
			route = routeLocator.getMatchingRoute(URLPATHHELPER.getPathWithinApplication(ctx.getRequest()));
			ctx.set(Constant.ROUTE, route);
		}
		return route;
	}

	/**
	 * 网关自己响应
	 * 
	 * @param result
	 */
	public static void responseBody(String result) {
		RequestContext ctx = RequestContext.getCurrentContext();
		ctx.addZuulResponseHeader(Constant.CONTENTTYPE, MediaType.APPLICATION_JSON_UTF8_VALUE);
		ctx.setSendZuulResponse(TrueFalseStatusEnum.FALSE.getValue());
		ctx.setResponseStatusCode(HttpServletResponse.SC_OK);
		String callback = ctx.getRequest().getParameter(Constant.CALLBACK);
		if (StringUtils.isNotEmpty(callback)) {
			int index = result.indexOf("({");
			if (index == -1) {
				ctx.setResponseBody(callback + "(" + result + ")");
			} else {
				ctx.setResponseBody(callback + result.substring(index));
			}
		} else {
			ctx.setResponseBody(result);
		}
	}
	
	/**
	 * 获取请求方法
	 * 
	 * @return
	 */
	public static String getMethod() {
		return RequestContext.getCurrentContext().getRequest().getMethod();
	}
	
	/**
	 * API网关信息放入threadLocal
	 * 
	 * @param apiGateway
	 */
	public static void setGatewayInfo(GatewayInfo gatewayInfo) {
		RequestContext.getCurrentContext().set("gatewayInfo", gatewayInfo);
	}
	
	/**
	 * 从threadLocal中获取API网关信息
	 * 
	 * @return
	 */
	public static GatewayInfo getGatewayInfo() {
		return (GatewayInfo) RequestContext.getCurrentContext().get("gatewayInfo");
	}
	
	/**
	 * 用户id放入threadLocal
	 * 
	 * @param userId
	 */
	public static void setUserId(Long userId) {
		RequestContext.getCurrentContext().set("userId", userId);
	}
	
	/**
	 * 从threadLocal中获取用户id
	 * 
	 * @return
	 */
	public static Long getUserId() {
		Long userId = (Long) RequestContext.getCurrentContext().get("userId");
		if (null == userId) {
			userId = Long.valueOf(0);
		}
		return userId;
	}
	
	/**
	 * RLock放入threadLocal
	 * 
	 * @param lock
	 */
	public static void setLock(RLock lock) {
		RequestContext.getCurrentContext().set("lock", lock);
	}
	
	/**
	 * 从threadLocal中获取RLock
	 * 
	 * @return
	 */
	public static RLock getLock() {
		return (RLock) RequestContext.getCurrentContext().get("lock");
	}
	
	/**
	 * 项目名称
	 * 
	 * @return
	 */
	public static String getProjectName() {
		return getGatewayInfo().getProjectInfo().getProjectName();
	}
	
	/**
	 * 是否登录
	 * 
	 * @return
	 */
	public static boolean isLogin() {
		if (isEnd()) {
			return false;
		}
		GatewayInfo gatewayInfo = getGatewayInfo();
		return null != gatewayInfo && gatewayInfo.getLogin();
	}
	
	/**
	 * 是否缓存
	 * 
	 * @return
	 */
	public static boolean isCache() {
		if (isEnd()) {
			return false;
		}
		GatewayInfo gatewayInfo = getGatewayInfo();
		return null != gatewayInfo && gatewayInfo.getCache() && null != gatewayInfo.getCacheInfo();
	}
	
	/**
	 * 是否加锁
	 * 
	 * @return
	 */
	public static boolean isLock() {
		if (isEnd()) {
			return false;
		}
		GatewayInfo gatewayInfo = getGatewayInfo();
		return null != gatewayInfo && gatewayInfo.getLock();
	}
	
	/**
	 * 是否鉴权
	 * 
	 * @return
	 */
	public static boolean isSign() {
		if (isEnd()) {
			return false;
		}
		GatewayInfo gatewayInfo = getGatewayInfo();
		return null != gatewayInfo && gatewayInfo.getSign() && null != gatewayInfo.getSignInfo();
	}
	
	/**
	 * 是否限流
	 * 
	 * @return
	 */
	public static boolean isRateLimit() {
		if (isEnd()) {
			return false;
		}
		GatewayInfo gatewayInfo = getGatewayInfo();
		return null != gatewayInfo && gatewayInfo.getRateLimit() && null != gatewayInfo.getRateLimitInfo();
	}

}
