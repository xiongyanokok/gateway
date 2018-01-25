package com.hexun.gateway.pojo;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2017年05月13日 上午10:35:00
 */
@Getter
@Setter
public class AggregatorInfo implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
     * 聚合名称
     */
	private String name;
	
	/**
	 * 聚合地址
	 */
	private String uri;
	
	/**
     * 聚合类型：1并行，2串行
     */
	private Integer type;
	
	/**
	 * 聚合资源
	 */
	private List<ResourceInfo> resourceInfos;

}
