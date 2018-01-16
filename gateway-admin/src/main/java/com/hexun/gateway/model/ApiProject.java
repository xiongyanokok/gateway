package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:07
 */
public class ApiProject implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 项目英文名称
     */
	private String projectEn;
	
	/**
     * 项目中文名称
     */
	private String projectCn;
	
	/**
     * 统一错误信息
     */
	private String errorMessage;
	
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
	
	public String getProjectEn() {
		return projectEn;
	}
	
	public void setProjectEn(String projectEn) {
		this.projectEn = projectEn;
	}
	
	public String getProjectCn() {
		return projectCn;
	}
	
	public void setProjectCn(String projectCn) {
		this.projectCn = projectCn;
	}
	
	public String getErrorMessage() {
		return errorMessage;
	}
	
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
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
        ApiProject other = (ApiProject) that;
    	return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))	
    		&& (this.getProjectEn() == null ? other.getProjectEn() == null : this.getProjectEn().equals(other.getProjectEn()))
    		&& (this.getProjectCn() == null ? other.getProjectCn() == null : this.getProjectCn().equals(other.getProjectCn()))
    		&& (this.getErrorMessage() == null ? other.getErrorMessage() == null : this.getErrorMessage().equals(other.getErrorMessage()))
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
        result = prime * result + ((getProjectEn() == null) ? 0 : getProjectEn().hashCode());
        result = prime * result + ((getProjectCn() == null) ? 0 : getProjectCn().hashCode());
        result = prime * result + ((getErrorMessage() == null) ? 0 : getErrorMessage().hashCode());
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
    	return "ApiProject [id=" + id + ", projectEn=" + projectEn + ", projectCn=" + projectCn + ", errorMessage=" + errorMessage + ", isDelete=" + isDelete + ", createUserId=" + createUserId + ", createTime=" + createTime + ", updateUserId=" + updateUserId + ", updateTime=" + updateTime + "]";
    }
        
}
