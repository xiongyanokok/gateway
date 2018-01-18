package com.hexun.gateway.model;

import java.io.Serializable;
import java.util.Date;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
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
	
	
	public Integer getId() {
		return id;
	}
	
	public void setId(Integer id) {
		this.id = id;
	}
	
	public Integer getAggregatorId() {
		return aggregatorId;
	}
	
	public void setAggregatorId(Integer aggregatorId) {
		this.aggregatorId = aggregatorId;
	}
	
	public Integer getResourceIndex() {
		return resourceIndex;
	}
	
	public void setResourceIndex(Integer resourceIndex) {
		this.resourceIndex = resourceIndex;
	}
	
	public String getResourceName() {
		return resourceName;
	}
	
	public void setResourceName(String resourceName) {
		this.resourceName = resourceName;
	}
	
	public String getResourceUrl() {
		return resourceUrl;
	}
	
	public void setResourceUrl(String resourceUrl) {
		this.resourceUrl = resourceUrl;
	}
	
	public Integer getResourceMethod() {
		return resourceMethod;
	}
	
	public void setResourceMethod(Integer resourceMethod) {
		this.resourceMethod = resourceMethod;
	}
	
	public Boolean getIsLogin() {
		return isLogin;
	}
	
	public void setIsLogin(Boolean isLogin) {
		this.isLogin = isLogin;
	}
	
	public Integer getTimeOut() {
		return timeOut;
	}
	
	public void setTimeOut(Integer timeOut) {
		this.timeOut = timeOut;
	}
	
	public Boolean getIsCache() {
		return isCache;
	}
	
	public void setIsCache(Boolean isCache) {
		this.isCache = isCache;
	}
	
	public Integer getCacheTime() {
		return cacheTime;
	}
	
	public void setCacheTime(Integer cacheTime) {
		this.cacheTime = cacheTime;
	}
	
	public String getResultTemplate() {
		return resultTemplate;
	}
	
	public void setResultTemplate(String resultTemplate) {
		this.resultTemplate = resultTemplate;
	}
	
	public String getParamTemplate() {
		return paramTemplate;
	}
	
	public void setParamTemplate(String paramTemplate) {
		this.paramTemplate = paramTemplate;
	}
	
	public String getDefaultValue() {
		return defaultValue;
	}
	
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
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
        AggregatorResource other = (AggregatorResource) that;
    	return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))	
    		&& (this.getAggregatorId() == null ? other.getAggregatorId() == null : this.getAggregatorId().equals(other.getAggregatorId()))
    		&& (this.getResourceIndex() == null ? other.getResourceIndex() == null : this.getResourceIndex().equals(other.getResourceIndex()))
    		&& (this.getResourceName() == null ? other.getResourceName() == null : this.getResourceName().equals(other.getResourceName()))
    		&& (this.getResourceUrl() == null ? other.getResourceUrl() == null : this.getResourceUrl().equals(other.getResourceUrl()))
    		&& (this.getResourceMethod() == null ? other.getResourceMethod() == null : this.getResourceMethod().equals(other.getResourceMethod()))
    		&& (this.getIsLogin() == null ? other.getIsLogin() == null : this.getIsLogin().equals(other.getIsLogin()))
    		&& (this.getTimeOut() == null ? other.getTimeOut() == null : this.getTimeOut().equals(other.getTimeOut()))
    		&& (this.getIsCache() == null ? other.getIsCache() == null : this.getIsCache().equals(other.getIsCache()))
    		&& (this.getCacheTime() == null ? other.getCacheTime() == null : this.getCacheTime().equals(other.getCacheTime()))
    		&& (this.getResultTemplate() == null ? other.getResultTemplate() == null : this.getResultTemplate().equals(other.getResultTemplate()))
    		&& (this.getParamTemplate() == null ? other.getParamTemplate() == null : this.getParamTemplate().equals(other.getParamTemplate()))
    		&& (this.getDefaultValue() == null ? other.getDefaultValue() == null : this.getDefaultValue().equals(other.getDefaultValue()))
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
        result = prime * result + ((getAggregatorId() == null) ? 0 : getAggregatorId().hashCode());
        result = prime * result + ((getResourceIndex() == null) ? 0 : getResourceIndex().hashCode());
        result = prime * result + ((getResourceName() == null) ? 0 : getResourceName().hashCode());
        result = prime * result + ((getResourceUrl() == null) ? 0 : getResourceUrl().hashCode());
        result = prime * result + ((getResourceMethod() == null) ? 0 : getResourceMethod().hashCode());
        result = prime * result + ((getIsLogin() == null) ? 0 : getIsLogin().hashCode());
        result = prime * result + ((getTimeOut() == null) ? 0 : getTimeOut().hashCode());
        result = prime * result + ((getIsCache() == null) ? 0 : getIsCache().hashCode());
        result = prime * result + ((getCacheTime() == null) ? 0 : getCacheTime().hashCode());
        result = prime * result + ((getResultTemplate() == null) ? 0 : getResultTemplate().hashCode());
        result = prime * result + ((getParamTemplate() == null) ? 0 : getParamTemplate().hashCode());
        result = prime * result + ((getDefaultValue() == null) ? 0 : getDefaultValue().hashCode());
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
    	return "AggregatorResource [id=" + id + ", aggregatorId=" + aggregatorId + ", resourceIndex=" + resourceIndex + ", resourceName=" + resourceName + ", resourceUrl=" + resourceUrl + ", resourceMethod=" + resourceMethod + ", isLogin=" + isLogin + ", timeOut=" + timeOut + ", isCache=" + isCache + ", cacheTime=" + cacheTime + ", resultTemplate=" + resultTemplate + ", paramTemplate=" + paramTemplate + ", defaultValue=" + defaultValue + ", isDelete=" + isDelete + ", createUserId=" + createUserId + ", createTime=" + createTime + ", updateUserId=" + updateUserId + ", updateTime=" + updateTime + "]";
    }
        
}
