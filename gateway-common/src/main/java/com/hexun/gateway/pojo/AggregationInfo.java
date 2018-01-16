package com.hexun.gateway.pojo;

import java.util.List;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2017年05月13日 上午10:35:00
 */
public class AggregationInfo {

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
	private List<AggregationResource> resources;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getUri() {
		return uri;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public Integer getType() {
		return type;
	}

	public void setType(Integer type) {
		this.type = type;
	}

	public List<AggregationResource> getResources() {
		return resources;
	}

	public void setResources(List<AggregationResource> resources) {
		this.resources = resources;
	}
	
	/**
	 * 聚合类型
	 */
	public enum AggregationType {

		/**
		 * 并行
		 */
		PARALLEL(1),
		
		/**
		 * 串行
		 */
		SERIAL(2);
		
		private Integer value;
		
		private AggregationType(Integer value){
			this.value= value;
		}

		public Integer getValue() {
			return value;
		}
		
	}

}
