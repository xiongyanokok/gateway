package com.hexun.gateway.config;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import com.hexun.gateway.common.Constant;
import com.hexun.gateway.common.utils.CommonUtils;
import com.hexun.gateway.exception.GatewayException;

import lombok.extern.slf4j.Slf4j;

/**
 * 全局异常处理
 * 
 * @author xiongyan
 * @date 2017年3月28日 下午4:15:46
 */
@ControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
	
	/**
	 * GatewayException 全局异常
	 * 
	 * @param e
	 * @param request
	 * @return
	 */
	@ExceptionHandler(GatewayException.class)
    @ResponseBody
    public Object exceptionHandler(GatewayException e, HttpServletRequest request) {
		log.error("GatewayException：", e);
		if (CommonUtils.isAjax(request)) {
			String message = e.getMessage();
			int index = message.indexOf('】');
			if (index > 0) {
				message = message.substring(index+1);
			}
			return errorJson(message);
		} else {
			return errorView(e);
		}
    } 
	
	/**
	 * RuntimeException 全局异常
	 * 
	 * @param e
	 * @param request
	 * @return
	 */
	@ExceptionHandler(RuntimeException.class)
    @ResponseBody
    public Object handleAllException(RuntimeException e, HttpServletRequest request) {
		log.error("RuntimeException：", e);
		if (CommonUtils.isAjax(request)) {
			return errorJson("系统错误");
		} else {
			return errorView(e);
		}
    } 
	
	/**
	 * 返回错误页面
	 * 
	 * @param e
	 * @return
	 */
	private ModelAndView errorView(Exception e) {
		ModelAndView mv = new ModelAndView(Constant.DEFAULT_ERROR_VIEW);
		mv.addObject(Constant.DEFAULT_MESSAGE, e.getMessage());
		return mv;
	}
	
	/**
	 * 返回错误json
	 * 
	 * @param message
	 * @return
	 */
	private Map<String, Object> errorJson(String message) {
		Map<String, Object> map = new HashMap<>(2);
		map.put(Constant.DEFAULT_CODE, "N");
		map.put(Constant.DEFAULT_MESSAGE, message);
        return map; 
	}
	
}
