package com.hexun.gateway.common;

import com.hexun.hwcommon.model.CommonLoginInfo;

import lombok.Getter;
import lombok.Setter;

/**
 * RequestContext
 * 
 * @author xiongyan
 * @date 2018年1月4日 下午4:31:40
 */
public class RequestContext {
	
	private RequestContext() {
		
	}
	
	private static final ThreadLocal<RequestContext> LOCALCONTEXT = new ThreadLocal<RequestContext>() {
		@Override
        protected RequestContext initialValue() {
            return new RequestContext();
        }
    };
	
	/**
	 * 用户登陆信息
	 */
    @Getter
    @Setter
	private CommonLoginInfo commonLoginInfo;
	
	public static RequestContext getCurrentContext() {
        return LOCALCONTEXT.get();
    }
	
	public static void remove() {
        LOCALCONTEXT.remove();
    }
	
}
