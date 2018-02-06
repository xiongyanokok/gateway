package com.hexun.gateway.controller;

import org.springframework.boot.autoconfigure.web.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.dianping.cat.message.Transaction;
import com.hexun.gateway.common.GatewayUtils;
import com.hexun.gateway.pojo.Result;
import com.netflix.zuul.context.RequestContext;

import lombok.extern.slf4j.Slf4j;

/**
 * 全局异常处理
 * 
 * @author xiongyan
 * @date 2017年12月20日 下午12:12:47
 */
@RestController
@Slf4j
public class ErrorHandlerController implements ErrorController {
	
	/**
	 * 有异常进入该方法，交由下面的方法处理
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
		Throwable e = ctx.getThrowable();
		try {
			log.error("系统错误：", e);
			if (HttpStatus.TOO_MANY_REQUESTS.value() == ctx.getResponseStatusCode()) {
				return Result.TOOOFTEN;
			}
			return Result.SYSTEMERROR;
		} finally {
			// 获取CAT Transaction
			Transaction transaction = GatewayUtils.getTransaction();
			if (null != transaction) {
				transaction.setStatus(e);
				transaction.complete();
			}
		}
	}
}
