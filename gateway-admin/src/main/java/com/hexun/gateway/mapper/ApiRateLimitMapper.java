package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiRateLimit;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:10
 */
public interface ApiRateLimitMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiRateLimit selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiRateLimit
     */
    void insert(ApiRateLimit apiRateLimit);

    /**
     * 更新数据库记录
     *
     * @param apiRateLimit
     */
    void update(ApiRateLimit apiRateLimit);

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
    void batchInsert(List<ApiRateLimit> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiRateLimit> list);
    
}
