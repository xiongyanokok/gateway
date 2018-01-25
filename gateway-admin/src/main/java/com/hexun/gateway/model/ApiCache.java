package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月04日 下午03:58:58
 */
@Data
public class ApiCache implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 网关id
     */
	private Integer apiId;
	
	/**
     * 缓存时间
     */
	private Long time;
	
	/**
     * 时间单位：1秒，2分，3时，4天
     */
	private Integer timeUnit;
	
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
