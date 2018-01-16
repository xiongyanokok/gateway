package com.hexun.gateway.exception;

import com.hexun.gateway.enums.ErrorCodeEnum;

/**
 * 自定义异常
 * 
 * @author xiongyan
 * @date 2017年6月6日 下午4:19:08
 */
public class GatewayException extends RuntimeException {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * 错误码
	 */
	private ErrorCodeEnum errorCode;
	
	public GatewayException() {
		 super();
	}

    public GatewayException(String message) {
        super(message);
    }
	
    public GatewayException(ErrorCodeEnum errorCode, String message) {
        super(message);
        this.errorCode = errorCode;
    }
    
    public GatewayException(String message, Throwable cause) {
    	super(message, cause);
    }
    
    public GatewayException(ErrorCodeEnum errorCode, String message, Throwable cause) {
    	super(message, cause);
    	this.errorCode = errorCode;
    }
    
    public GatewayException(Throwable cause) {
    	super(cause);
    }
    
	public ErrorCodeEnum getErrorCode() {
		return errorCode;
	}

}
