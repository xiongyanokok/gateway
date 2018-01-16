package com.hexun.gateway.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.hexun.gateway.pojo.Result;
import com.netflix.zuul.context.RequestContext;

/**
 * 全局异常处理
 * 
 * @author xiongyan
 * @date 2017年12月20日 下午12:12:47
 */
@RestController
public class ErrorHandlerController implements ErrorController {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ErrorHandlerController	.class);

	/**
	 * 发现异常后进入该方法，交由下面的方法处理
	 * 
	 * @return
	 */
	@Override
	public String getErrorPath() {
		return "/error";
	}

	/**
	 * 统一错误处理
	 * 
	 * @return
	 */
	@RequestMapping(value = "/error", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Result error() {
		RequestContext ctx = RequestContext.getCurrentContext();
		logger.error("系统错误：", ctx.getThrowable());
		if (HttpStatus.TOO_MANY_REQUESTS.value() == ctx.getResponseStatusCode()) {
			return Result.TOOOFTEN;
		}
		return Result.SYSTEMERROR;
	}
}
