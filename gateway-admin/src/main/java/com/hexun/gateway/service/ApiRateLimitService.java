package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiRateLimit;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:10
 */
public interface ApiRateLimitService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiRateLimit selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiRateLimit getApiRateLimitById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiRateLimit
     */
    void save(ApiRateLimit apiRateLimit);

    /**
     * 修改数据
     *
     * @param apiRateLimit
     */
    void update(ApiRateLimit apiRateLimit);
    
    /**
     * 删除数据
     * 
     * @param apiRateLimit
     */
    void remove(ApiRateLimit apiRateLimit);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiRateLimit getApiRateLimit(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiRateLimit> listApiRateLimit(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ApiRateLimit> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiRateLimit> list);
    
}
