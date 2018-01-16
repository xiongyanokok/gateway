package com.hexun.gateway.config;

import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.AbstractJsonpResponseBodyAdvice;

import com.hexun.gateway.common.Constant;

/**
 * 支持跨域请求的JSONP数据
 * 
 * @author xiongyan
 * @date 2017年12月20日 下午12:35:07
 */
@ControllerAdvice
public class JsonpConfiguration extends AbstractJsonpResponseBodyAdvice {

	public JsonpConfiguration() {
		super(Constant.CALLBACK, "jsonp");
	}

}
