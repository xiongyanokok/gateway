package com.hexun.gateway.enums;

/**
 * 聚合类型
 * 
 * @author xiongyan
 * @date 2017年6月6日 下午4:19:08
 */
public enum AggregatorTypeEnum {

	/**
	 * 并行
	 */
	PARALLEL(1),
	
	/**
	 * 串行
	 */
	SERIAL(2);
	
	private Integer value;
	
	private AggregatorTypeEnum(Integer value){
		this.value= value;
	}

	public Integer getValue() {
		return value;
	}
	
}