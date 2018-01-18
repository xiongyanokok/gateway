package com.hexun.gateway.pojo;

import java.io.Serializable;
import java.util.Map;

/**
 * model 实体类
 *
 * @author admin
 * @date 2017年05月13日 上午10:35:00
 */
public class ResourceInfo implements Comparable<ResourceInfo>, Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 聚合名称
     */
	private String name;

	/**
     * 资源序号
     */
    private Integer resourceIndex;

    /**
     * 资源名称
     */
    private String resourceName;

    /**
     * 资源url
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
     * 资源默认值
     */
    private String defaultValue;
    
    /**
     * cookie
     */
    private String cookie;
    
    /**
     * 请求参数
     */
    private Map<String, String> paramMap;
    
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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

	public String getCookie() {
		return cookie;
	}

	public void setCookie(String cookie) {
		this.cookie = cookie;
	}

	public Map<String, String> getParamMap() {
		return paramMap;
	}

	public void setParamMap(Map<String, String> paramMap) {
		this.paramMap = paramMap;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	/**
     * 根据序号排序 （从小到大）
     */
    @Override
    public int compareTo(ResourceInfo resourceInfo) {
        return this.getResourceIndex().compareTo(resourceInfo.getResourceIndex());
    }

}
