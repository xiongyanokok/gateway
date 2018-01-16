package com.hexun.gateway.common;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.springframework.web.util.HtmlUtils;

/**
 * 解决XSS跨站脚本攻击和sql注入攻击
 * 
 * @author xiongyan
 * @date 2017年12月25日 下午12:01:02
 */
public class XssSpringHttpServletRequestWrapper extends HttpServletRequestWrapper {

	public XssSpringHttpServletRequestWrapper(HttpServletRequest request) {
		super(request);
	}

	/**
	 * 对数组参数进行特殊字符过滤
	 * 
	 * @param name
	 */
	@Override
	public String[] getParameterValues(String name) {
		String[] values = super.getParameterValues(name);
		String[] newValues = new String[values.length];
		for (int i = 0; i < values.length; i++) {
			newValues[i] = HtmlUtils.htmlEscape(values[i]);
		}
		return newValues;
	}

	/**
	 * 拦截参数,并对其进行字符转义
	 * 
	 * @param name
	 */
	@Override
	public String getParameter(String name) {
		return HtmlUtils.htmlEscape(super.getParameter(name));
	}

	/**
	 * 拦截参数,并对其进行字符转义
	 * 
	 * @param name
	 */
	@Override
	public Object getAttribute(String name) {
		return HtmlUtils.htmlEscape(super.getParameter(name));
	}

}
