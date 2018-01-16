package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiCache;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:58:58
 */
public interface ApiCacheMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiCache selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiCache
     */
    void insert(ApiCache apiCache);

    /**
     * 更新数据库记录
     *
     * @param apiCache
     */
    void update(ApiCache apiCache);

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
    void batchInsert(List<ApiCache> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiCache> list);
    
}
