package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月08日 下午01:32:41
 */
@Data
public class ApiRateLimit implements Serializable {

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
     * 请求数量限制
     */
	private Long numLimit;
	
	/**
     * 请求时间限制(秒)
     */
	private Long timeQuota;
	
	/**
     * 刷新间隔(秒)
     */
	private Long refreshInterval;
	
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
