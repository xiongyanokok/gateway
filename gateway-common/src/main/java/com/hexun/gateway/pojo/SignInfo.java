package com.hexun.gateway.pojo;

import lombok.Getter;
import lombok.Setter;

/**
 * 签名信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:33
 */
@Getter
@Setter
public class SignInfo {

	/**
	 * 签名字段
	 */
	private String signField;
	
	/**
	 * 签名方法
	 */
	private Integer signMethod;
	
	/**
	 * 签名KEY
	 */
	private String signKey;

}
