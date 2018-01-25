package com.hexun.gateway.pojo;

import java.io.Serializable;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

/**
 * model 实体类
 *
 * @author admin
 * @date 2017年05月13日 上午10:35:00
 */
@Getter
@Setter
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
    
	/**
     * 根据序号排序 （从小到大）
     */
    @Override
    public int compareTo(ResourceInfo resourceInfo) {
        return this.getResourceIndex().compareTo(resourceInfo.getResourceIndex());
    }

}
