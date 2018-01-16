package com.hexun.gateway.pojo;

import java.io.Serializable;
import java.util.Map;

/**
 * model 实体类
 *
 * @author admin
 * @date 2017年05月13日 上午10:35:00
 */
public class AggregationResource implements Comparable<AggregationResource>, Serializable {

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
     * 请求方法：GET/POST
     */
    private String resourceMethod;

    /**
     * 是否登录：1是，0否
     */
    private Boolean isLogin;

    /**
     * cookie
     */
    private String cookie;
    
    /**
     * 请求参数
     */
    private Map<String, String> paramMap;
    
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
     * 资源模板
     */
    private String resourceTemplate;

    /**
     * 正则表达式
     */
    private String resourceRegex;

    /**
     * 资源分支
     */
    private String resourceBranchs;
    
    /**
     * 资源默认值
     */
    private String defaultValue;
    
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

	public String getResourceMethod() {
		return resourceMethod;
	}

	public void setResourceMethod(String resourceMethod) {
		this.resourceMethod = resourceMethod;
	}

	public Boolean getIsLogin() {
		return isLogin;
	}

	public void setIsLogin(Boolean isLogin) {
		this.isLogin = isLogin;
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

	public String getResourceTemplate() {
		return resourceTemplate;
	}

	public void setResourceTemplate(String resourceTemplate) {
		this.resourceTemplate = resourceTemplate;
	}

	public String getResourceRegex() {
		return resourceRegex;
	}

	public void setResourceRegex(String resourceRegex) {
		this.resourceRegex = resourceRegex;
	}

	public String getResourceBranchs() {
		return resourceBranchs;
	}

	public void setResourceBranchs(String resourceBranchs) {
		this.resourceBranchs = resourceBranchs;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
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
    public int compareTo(AggregationResource resourceInfo) {
        return this.getResourceIndex().compareTo(resourceInfo.getResourceIndex());
    }

}
