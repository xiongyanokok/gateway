package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月05日 下午04:26:21
 */
@Data
public class ApiInfo implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 所属项目
     */
	private Integer projectId;
	
	/**
     * API地址
     */
	private String uri;
	
	/**
     * 是否启用：1是，0否
     */
	private Boolean enabled;
	
	/**
     * 方法：1get，2post，3get或post
     */
	private Integer method;
	
	/**
     * 是否登录：1是，0否
     */
	private Boolean login;
	
	/**
     * 是否鉴权：1是，0否
     */
	private Boolean sign;
	
	/**
     * 是否分布式缓存：1是，0否
     */
	private Boolean globalCache;
	
	/**
     * 是否分布式锁：1是，0否
     */
	private Boolean globalLock;
	
	/**
     * 是否限流：1是，0否
     */
	private Boolean rateLimit;
	
	/**
     * 是否开启监控：1是，0否
     */
	private Boolean monitor;
	
	/**
     * 是否记录日志：1是，0否
     */
	private Boolean log;
	
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
	
	/**
	 * 项目信息
	 */
    private ApiProject projectInfo;
    
    /**
     * 缓存信息
     */
    private ApiCache cacheInfo;
    
    /**
     * 鉴权信息
     */
    private ApiSign signInfo;
    
    /**
     * 限流信息
     */
    private ApiRateLimit rateLimitInfo;
    
    /**
     * ip黑名单
     */
    private List<String> ips;
    
    /**
     * 用户黑名单
     */
    private List<Long> userIds;

}
