package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:40
 */
@Data
public class Aggregator implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
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
     * 聚合描述
     */
	private String description;
	
	/**
     * 是否删除：1是，0否
     */
	private Boolean isDelete;
	
	/**
     * 创建人id
     */
	private Long createUserId;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
	/**
     * 修改人id
     */
	private Long updateUserId;
	
	/**
     * 修改时间
     */
	private Date updateTime;
	
}
