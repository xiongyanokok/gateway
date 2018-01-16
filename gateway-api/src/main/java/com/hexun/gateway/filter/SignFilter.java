package com.hexun.gateway.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;

import com.hexun.common.security.Md5Utils;
import com.hexun.common.utils.SignParameterUtils;
import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.Result;
import com.hexun.gateway.pojo.SignInfo;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

/**
 * 鉴权过滤器
 * 
 * @author xiongyan
 * @date 2017年12月18日 下午1:55:49
 */
public class SignFilter extends ZuulFilter {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(SignFilter.class);

	/**
	 * 是否执行该过滤器
	 */
	@Override
	public boolean shouldFilter() {
		return GatewayUtils.isSign();
	}

	/**
	 * 执行
	 */
	@Override
	public Object run() {
		HttpServletRequest request = RequestContext.getCurrentContext().getRequest();
		// 鉴权信息
		SignInfo signInfo = GatewayUtils.getGatewayInfo().getSignInfo();
		// 签名字段
		String signField = signInfo.getSignField();
		// 签名内容
		String signValue = request.getParameter(signField);
		if (StringUtils.isEmpty(signValue)) {
			logger.error("鉴权失败：签名字段【{}】不存在", signField);
			GatewayUtils.responseBody(Result.SIGNERROR.toString());
			return null;
		}
		
		// 请求参数
		Map<String, String[]> map = request.getParameterMap();
		Map<String, Object> paramsMap = new HashMap<>();
		for (Entry<String, String[]> entry : map.entrySet()) {
			String key = entry.getKey();
			String value = entry.getValue()[0];
			if ("callback".equals(key) || "_".equals(key) || signField.equals(key)) {
				continue;
			} else {
				paramsMap.put(key, value);
			}
		}
		
		// 生成MD5密钥
		String md5Sign = Md5Utils.md5(SignParameterUtils.createSignText(paramsMap) + signInfo.getSignKey());
		if (StringUtils.isEmpty(md5Sign) || !md5Sign.equals(signValue)) {
			logger.error("鉴权失败：请求URI【{}】，请求参数【{}】，请求密钥【{}】，MD5密钥【{}】", request.getRequestURI(), paramsMap, signValue, md5Sign);
			GatewayUtils.responseBody(Result.SIGNERROR.toString());
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
		return 3;
	}
	
}
