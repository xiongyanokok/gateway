package com.hexun.gateway.pojo;

/**
 * 签名信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:33
 */
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

	public String getSignField() {
		return signField;
	}

	public void setSignField(String signField) {
		this.signField = signField;
	}

	public Integer getSignMethod() {
		return signMethod;
	}

	public void setSignMethod(Integer signMethod) {
		this.signMethod = signMethod;
	}

	public String getSignKey() {
		return signKey;
	}

	public void setSignKey(String signKey) {
		this.signKey = signKey;
	}
	
}
