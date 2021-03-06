package com.hexun.gateway.filter;

import javax.servlet.http.HttpServletRequest;

import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.hexun.gateway.common.XssSpringHttpServletRequestWrapper;
import com.hexun.gateway.common.GatewayUtils;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

/**
 * Xss过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午2:02:45
 */
@Component
public class XssFilter extends ZuulFilter {
	
	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return GatewayUtils.isXss();
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		RequestContext ctx = RequestContext.getCurrentContext();
		HttpServletRequest request = ctx.getRequest();
		ctx.setRequest(new XssSpringHttpServletRequestWrapper(request));
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
		return 2;
	}

}
