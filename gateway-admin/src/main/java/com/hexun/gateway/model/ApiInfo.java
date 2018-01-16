package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月05日 下午04:26:21
 */
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
	
	
	public Integer getId() {
		return id;
	}
	
	public void setId(Integer id) {
		this.id = id;
	}
	
	public Integer getProjectId() {
		return projectId;
	}
	
	public void setProjectId(Integer projectId) {
		this.projectId = projectId;
	}
	
	public String getUri() {
		return uri;
	}
	
	public void setUri(String uri) {
		this.uri = uri;
	}
	
	public Boolean getEnabled() {
		return enabled;
	}
	
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}
	
	public Integer getMethod() {
		return method;
	}
	
	public void setMethod(Integer method) {
		this.method = method;
	}
	
	public Boolean getLogin() {
		return login;
	}
	
	public void setLogin(Boolean login) {
		this.login = login;
	}
	
	public Boolean getSign() {
		return sign;
	}
	
	public void setSign(Boolean sign) {
		this.sign = sign;
	}
	
	public Boolean getGlobalCache() {
		return globalCache;
	}
	
	public void setGlobalCache(Boolean globalCache) {
		this.globalCache = globalCache;
	}
	
	public Boolean getGlobalLock() {
		return globalLock;
	}
	
	public void setGlobalLock(Boolean globalLock) {
		this.globalLock = globalLock;
	}
	
	public Boolean getRateLimit() {
		return rateLimit;
	}
	
	public void setRateLimit(Boolean rateLimit) {
		this.rateLimit = rateLimit;
	}
	
	public Boolean getMonitor() {
		return monitor;
	}
	
	public void setMonitor(Boolean monitor) {
		this.monitor = monitor;
	}
	
	public Boolean getLog() {
		return log;
	}
	
	public void setLog(Boolean log) {
		this.log = log;
	}
	
	public Boolean getIsDelete() {
		return isDelete;
	}
	
	public void setIsDelete(Boolean isDelete) {
		this.isDelete = isDelete;
	}
	
	public Long getCreateUserId() {
		return createUserId;
	}
	
	public void setCreateUserId(Long createUserId) {
		this.createUserId = createUserId;
	}
	
	public Date getCreateTime() {
		return createTime;
	}
	
	public void setCreateTime(Date createTime) {
		this.createTime = createTime;
	}
	
	public Long getUpdateUserId() {
		return updateUserId;
	}
	
	public void setUpdateUserId(Long updateUserId) {
		this.updateUserId = updateUserId;
	}
	
	public Date getUpdateTime() {
		return updateTime;
	}
	
	public void setUpdateTime(Date updateTime) {
		this.updateTime = updateTime;
	}
    
    
    /**
     * equals
     *
     * @param that
     */
    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (that == null) {
            return false;
        }
        if (getClass() != that.getClass()) {
            return false;
        }
        ApiInfo other = (ApiInfo) that;
    	return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))	
    		&& (this.getProjectId() == null ? other.getProjectId() == null : this.getProjectId().equals(other.getProjectId()))
    		&& (this.getUri() == null ? other.getUri() == null : this.getUri().equals(other.getUri()))
    		&& (this.getEnabled() == null ? other.getEnabled() == null : this.getEnabled().equals(other.getEnabled()))
    		&& (this.getMethod() == null ? other.getMethod() == null : this.getMethod().equals(other.getMethod()))
    		&& (this.getLogin() == null ? other.getLogin() == null : this.getLogin().equals(other.getLogin()))
    		&& (this.getSign() == null ? other.getSign() == null : this.getSign().equals(other.getSign()))
    		&& (this.getGlobalCache() == null ? other.getGlobalCache() == null : this.getGlobalCache().equals(other.getGlobalCache()))
    		&& (this.getGlobalLock() == null ? other.getGlobalLock() == null : this.getGlobalLock().equals(other.getGlobalLock()))
    		&& (this.getRateLimit() == null ? other.getRateLimit() == null : this.getRateLimit().equals(other.getRateLimit()))
    		&& (this.getMonitor() == null ? other.getMonitor() == null : this.getMonitor().equals(other.getMonitor()))
    		&& (this.getLog() == null ? other.getLog() == null : this.getLog().equals(other.getLog()))
    		&& (this.getIsDelete() == null ? other.getIsDelete() == null : this.getIsDelete().equals(other.getIsDelete()))
    		&& (this.getCreateUserId() == null ? other.getCreateUserId() == null : this.getCreateUserId().equals(other.getCreateUserId()))
    		&& (this.getCreateTime() == null ? other.getCreateTime() == null : this.getCreateTime().equals(other.getCreateTime()))
    		&& (this.getUpdateUserId() == null ? other.getUpdateUserId() == null : this.getUpdateUserId().equals(other.getUpdateUserId()))
    		&& (this.getUpdateTime() == null ? other.getUpdateTime() == null : this.getUpdateTime().equals(other.getUpdateTime()));
    }

    /**
     * hashCode
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((getId() == null) ? 0 : getId().hashCode());
        result = prime * result + ((getProjectId() == null) ? 0 : getProjectId().hashCode());
        result = prime * result + ((getUri() == null) ? 0 : getUri().hashCode());
        result = prime * result + ((getEnabled() == null) ? 0 : getEnabled().hashCode());
        result = prime * result + ((getMethod() == null) ? 0 : getMethod().hashCode());
        result = prime * result + ((getLogin() == null) ? 0 : getLogin().hashCode());
        result = prime * result + ((getSign() == null) ? 0 : getSign().hashCode());
        result = prime * result + ((getGlobalCache() == null) ? 0 : getGlobalCache().hashCode());
        result = prime * result + ((getGlobalLock() == null) ? 0 : getGlobalLock().hashCode());
        result = prime * result + ((getRateLimit() == null) ? 0 : getRateLimit().hashCode());
        result = prime * result + ((getMonitor() == null) ? 0 : getMonitor().hashCode());
        result = prime * result + ((getLog() == null) ? 0 : getLog().hashCode());
        result = prime * result + ((getIsDelete() == null) ? 0 : getIsDelete().hashCode());
        result = prime * result + ((getCreateUserId() == null) ? 0 : getCreateUserId().hashCode());
        result = prime * result + ((getCreateTime() == null) ? 0 : getCreateTime().hashCode());
        result = prime * result + ((getUpdateUserId() == null) ? 0 : getUpdateUserId().hashCode());
        result = prime * result + ((getUpdateTime() == null) ? 0 : getUpdateTime().hashCode());
        return result;
    }
    
    /**
     * toString
     */
    @Override
    public String toString() {
    	return "ApiInfo [id=" + id + ", projectId=" + projectId + ", uri=" + uri + ", enabled=" + enabled + ", method=" + method + ", login=" + login + ", sign=" + sign + ", globalCache=" + globalCache + ", globalLock=" + globalLock + ", rateLimit=" + rateLimit + ", monitor=" + monitor + ", log=" + log + ", isDelete=" + isDelete + ", createUserId=" + createUserId + ", createTime=" + createTime + ", updateUserId=" + updateUserId + ", updateTime=" + updateTime + "]";
    }
    
    private ApiProject projectInfo;
    
    private ApiCache cacheInfo;
    
    private ApiSign signInfo;
    
    private ApiRateLimit rateLimitInfo;
    
    private List<String> ips;
    
    private List<Long> userIds;


	public ApiProject getProjectInfo() {
		return projectInfo;
	}

	public void setProjectInfo(ApiProject projectInfo) {
		this.projectInfo = projectInfo;
	}

	public ApiCache getCacheInfo() {
		return cacheInfo;
	}

	public void setCacheInfo(ApiCache cacheInfo) {
		this.cacheInfo = cacheInfo;
	}

	public ApiSign getSignInfo() {
		return signInfo;
	}

	public void setSignInfo(ApiSign signInfo) {
		this.signInfo = signInfo;
	}

	public ApiRateLimit getRateLimitInfo() {
		return rateLimitInfo;
	}

	public void setRateLimitInfo(ApiRateLimit rateLimitInfo) {
		this.rateLimitInfo = rateLimitInfo;
	}

	public List<String> getIps() {
		return ips;
	}

	public void setIps(List<String> ips) {
		this.ips = ips;
	}

	public List<Long> getUserIds() {
		return userIds;
	}

	public void setUserIds(List<Long> userIds) {
		this.userIds = userIds;
	}

}
