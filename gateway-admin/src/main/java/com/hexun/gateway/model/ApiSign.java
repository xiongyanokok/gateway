package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
@Data
public class ApiSign implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * id
     */
	private Integer id;
	
	/**
     * 网关id
     */
	private Integer apiId;
	
	/**
     * 签名字段
     */
	private String signField;
	
	/**
     * 签名方法：1，2，3
     */
	private Integer signMethod;
	
	/**
     * 签名key
     */
	private String signKey;
	
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
