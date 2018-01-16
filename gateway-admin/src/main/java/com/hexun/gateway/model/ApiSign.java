package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
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
	
	public String getSignField() {
		return signField;
	}
	
	public void setSignField(String signField) {
		this.signField = signField;
	}
	
	public Integer getSignMethod() {
		return signMethod;
	}
	
	public void setSignMethod(Integer signMethod) {
		this.signMethod = signMethod;
	}
	
	public String getSignKey() {
		return signKey;
	}
	
	public void setSignKey(String signKey) {
		this.signKey = signKey;
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
        ApiSign other = (ApiSign) that;
    	return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))	
    		&& (this.getApiId() == null ? other.getApiId() == null : this.getApiId().equals(other.getApiId()))
    		&& (this.getSignField() == null ? other.getSignField() == null : this.getSignField().equals(other.getSignField()))
    		&& (this.getSignMethod() == null ? other.getSignMethod() == null : this.getSignMethod().equals(other.getSignMethod()))
    		&& (this.getSignKey() == null ? other.getSignKey() == null : this.getSignKey().equals(other.getSignKey()))
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
        result = prime * result + ((getSignField() == null) ? 0 : getSignField().hashCode());
        result = prime * result + ((getSignMethod() == null) ? 0 : getSignMethod().hashCode());
        result = prime * result + ((getSignKey() == null) ? 0 : getSignKey().hashCode());
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
    	return "ApiSign [id=" + id + ", apiId=" + apiId + ", signField=" + signField + ", signMethod=" + signMethod + ", signKey=" + signKey + ", isDelete=" + isDelete + ", createUserId=" + createUserId + ", createTime=" + createTime + ", updateUserId=" + updateUserId + ", updateTime=" + updateTime + "]";
    }
        
}
