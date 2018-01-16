package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiCache;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:58:58
 */
public interface ApiCacheService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiCache selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiCache getApiCacheById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiCache
     */
    void save(ApiCache apiCache);

    /**
     * 修改数据
     *
     * @param apiCache
     */
    void update(ApiCache apiCache);
    
    /**
     * 删除数据
     * 
     * @param apiCache
     */
    void remove(ApiCache apiCache);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiCache getApiCache(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiCache> listApiCache(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ApiCache> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiCache> list);
    
}
