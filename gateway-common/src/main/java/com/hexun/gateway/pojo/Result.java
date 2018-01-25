package com.hexun.gateway.pojo;

import lombok.Getter;
import lombok.Setter;

/**
 * 响应结果
 * 
 * @author hexun
 * @date 2016年10月18日 下午3:16:45
 */
@Getter
@Setter
public class Result {
	
    /**
     * 错误码
     */
    private Integer code;

    /**
     * 描述信息
     */
    private String msg;

    /**
	 * 默认构造方法
	 */
	public Result() {
		
	}

    /**
     * @param code
     * @param msg
     */
    public Result(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    @Override
    public String toString() {
		return "{\"code\":" + code + ", \"msg\":\"" + msg + "\"}";
    }

    public static final Result BUSYERROR = new Result(-1, "系统繁忙，请稍候重试");
    public static final Result SUCCESS = new Result(0, "成功");
    public static final Result SYSTEMERROR = new Result(10001, "系统错误");
    public static final Result NOTLOGIN = new Result(10002, "未登录");
    public static final Result SERVERERROR = new Result(10003, "服务不可用");
    public static final Result NORESOURCE = new Result(10004, "资源不存在");
    public static final Result SIGNERROR = new Result(10005, "签权失败");
    public static final Result TOOOFTEN = new Result(10006, "请求太频繁");
    public static final Result BLACKLIST = new Result(10007, "黑名单");
    public static final Result NOTSUPPORT = new Result(10008, "请求方法不支持");
    
}
