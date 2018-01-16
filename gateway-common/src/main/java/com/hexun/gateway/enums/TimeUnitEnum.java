package com.hexun.gateway.enums;

import java.util.concurrent.TimeUnit;

/**
 * 时间单位
 * 
 * @author xiongyan
 * @date 2016年11月21日 上午11:39:33
 */
public enum TimeUnitEnum {

	/**
	 * 秒
	 */
	SECONDS(1, TimeUnit.SECONDS),
	
	/**
	 * 分
	 */
	MINUTES(2, TimeUnit.MINUTES),
	
	/**
	 * 时
	 */
	HOURS(3, TimeUnit.HOURS),
	
	/**
	 * 天
	 */
	DAYS(4, TimeUnit.DAYS);
	
    private Integer value;
    
    private TimeUnit timeUnit;
	
    private TimeUnitEnum(Integer value, TimeUnit timeUnit) {
		this.value = value;
		this.timeUnit = timeUnit;
	}
    
	public Integer getValue() {
		return this.value;
	}
	
	public TimeUnit getTimeUnit() {
		return timeUnit;
	}

	public static TimeUnit getTimeUnit(Integer value) {
		for (TimeUnitEnum timeUnit : TimeUnitEnum.values()) {
			if (value.equals(timeUnit.getValue())) {
				return timeUnit.getTimeUnit();
			}
		}
		return null;
	}
}
