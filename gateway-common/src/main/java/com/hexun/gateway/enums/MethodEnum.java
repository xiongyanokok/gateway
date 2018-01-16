package com.hexun.gateway.enums;

/**
 * 请求方法
 * 
 * @author xiongyan
 * @date 2016年11月21日 上午11:39:33
 */
public enum MethodEnum {

	/**
	 * GET
	 */
	GET(1),
	
	/**
	 * POST
	 */
	POST(2);
	
    private Integer value;
	
    private MethodEnum(Integer value) {
		this.value = value;
	}
    
	public Integer getValue() {
		return this.value;
	}
	
	public static String getMethod(Integer value) {
		for (MethodEnum method : MethodEnum.values()) {
			if (value.equals(method.getValue())) {
				return method.name();
			}
		}
		return null;
	}
}
