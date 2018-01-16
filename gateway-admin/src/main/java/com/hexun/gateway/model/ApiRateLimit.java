package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月08日 下午01:32:41
 */
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
	
	
	public Integer getId() {
		return id;
	}
	
	public void setId(Integer id) {
		this.id = id;
	}
	
	public Integer getApiId() {
		return apiId;
	}
	
	public void setApiId(Integer apiId) {
		this.apiId = apiId;
	}
	
	public Long getNumLimit() {
		return numLimit;
	}
	
	public void setNumLimit(Long numLimit) {
		this.numLimit = numLimit;
	}
	
	public Long getTimeQuota() {
		return timeQuota;
	}
	
	public void setTimeQuota(Long timeQuota) {
		this.timeQuota = timeQuota;
	}
	
	public Long getRefreshInterval() {
		return refreshInterval;
	}
	
	public void setRefreshInterval(Long refreshInterval) {
		this.refreshInterval = refreshInterval;
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
        ApiRateLimit other = (ApiRateLimit) that;
    	return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))	
    		&& (this.getApiId() == null ? other.getApiId() == null : this.getApiId().equals(other.getApiId()))
    		&& (this.getNumLimit() == null ? other.getNumLimit() == null : this.getNumLimit().equals(other.getNumLimit()))
    		&& (this.getTimeQuota() == null ? other.getTimeQuota() == null : this.getTimeQuota().equals(other.getTimeQuota()))
    		&& (this.getRefreshInterval() == null ? other.getRefreshInterval() == null : this.getRefreshInterval().equals(other.getRefreshInterval()))
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
        result = prime * result + ((getApiId() == null) ? 0 : getApiId().hashCode());
        result = prime * result + ((getNumLimit() == null) ? 0 : getNumLimit().hashCode());
        result = prime * result + ((getTimeQuota() == null) ? 0 : getTimeQuota().hashCode());
        result = prime * result + ((getRefreshInterval() == null) ? 0 : getRefreshInterval().hashCode());
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
    	return "ApiRateLimit [id=" + id + ", apiId=" + apiId + ", numLimit=" + numLimit + ", timeQuota=" + timeQuota + ", refreshInterval=" + refreshInterval + ", isDelete=" + isDelete + ", createUserId=" + createUserId + ", createTime=" + createTime + ", updateUserId=" + updateUserId + ", updateTime=" + updateTime + "]";
    }
        
}
