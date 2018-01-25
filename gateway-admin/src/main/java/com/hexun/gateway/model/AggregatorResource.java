package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
@Data
public class AggregatorResource implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 聚合id
     */
	private Integer aggregatorId;
	
	/**
     * 资源序号
     */
	private Integer resourceIndex;
	
	/**
     * 资源名称
     */
	private String resourceName;
	
	/**
     * 资源接口地址
     */
	private String resourceUrl;
	
	/**
     * 请求方法：1GET，2POST
     */
	private Integer resourceMethod;
	
	/**
     * 是否登录：1是，0否
     */
	private Boolean isLogin;
	
	/**
     * 超时时间：单位秒
     */
	private Integer timeOut;
	
	/**
     * 是否缓存：1是，0否
     */
	private Boolean isCache;
	
	/**
     * 缓存时间：单位分钟
     */
	private Integer cacheTime;
	
	/**
     * 结果模板
     */
	private String resultTemplate;
	
	/**
     * 参数模板
     */
	private String paramTemplate;
	
	/**
     * 默认值
     */
	private String defaultValue;
	
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
