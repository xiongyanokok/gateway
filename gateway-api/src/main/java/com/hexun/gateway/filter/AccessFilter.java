package com.hexun.gateway.filter;

import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cloud.netflix.zuul.filters.Route;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.dianping.cat.Cat;
import com.dianping.cat.CatConstants;
import com.dianping.cat.message.Transaction;
import com.hexun.common.utils.IpUtils;
import com.hexun.gateway.common.CatUtils;
import com.hexun.gateway.common.GatewayCache;
import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.GatewayInfo;
import com.hexun.gateway.pojo.Result;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

import lombok.extern.slf4j.Slf4j;

/**
 * 访问过滤器
 * 1，资源过滤
 * 2，开关控制
 * 3，黑名单过滤
 * 
 * @author xiongyan
 * @date 2017年12月18日 上午11:01:57
 */
@Component
@Slf4j
public class AccessFilter extends ZuulFilter {
	
	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return true;
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		Route route = GatewayUtils.getRoute();
		String fullPath = route.getFullPath();
		// API网关
		GatewayInfo apiGateway = GatewayCache.get(fullPath);
		
		// 网关
		if (null == apiGateway) {
			GatewayUtils.responseBody(Result.NORESOURCE.toString());
			log.info("网关地址【{}】不存在", fullPath);
			return null;
		}
		
		// 开关
		if (!apiGateway.getEnabled()) {
			GatewayUtils.responseBody(Result.SERVERERROR.toString());
			log.info("网关地址【{}】不可用", fullPath);
			return null;
		}
		
		// 方法
		String method = apiGateway.getMethod();
		if (StringUtils.isNotEmpty(method) && !method.equalsIgnoreCase(GatewayUtils.getMethod())) {
			GatewayUtils.responseBody(Result.NOTSUPPORT.toString());
			log.info("网关地址【{}】方法【{}】不支持目标方法【{}】", fullPath, GatewayUtils.getMethod(), method);
			return null;
		}
		
		// ip黑名单
		Set<String> ips = apiGateway.getIps();
		if (CollectionUtils.isNotEmpty(ips)) {
			String ip = IpUtils.getHostIP();
			if (ips.contains(ip)) {
				GatewayUtils.responseBody(Result.BLACKLIST.toString());
				log.info("IP地址【{}】已列入黑名单", ip);
				return null;
			}
		}
		
		// CAT监控
		Transaction transaction = Cat.newTransaction(CatConstants.TYPE_URL, fullPath);
		CatUtils.catLog(RequestContext.getCurrentContext().getRequest());
		
		// API网关信息放入threadLocal
		GatewayUtils.setGatewayInfo(apiGateway);
		// Transaction放入threadLocal
		GatewayUtils.setTransaction(transaction);
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
		return -2;
	}

}
