package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.AggregatorResource;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
public interface AggregatorResourceMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    AggregatorResource selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param aggregatorResource
     */
    void insert(AggregatorResource aggregatorResource);

    /**
     * 更新数据库记录
     *
     * @param aggregatorResource
     */
    void update(AggregatorResource aggregatorResource);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    AggregatorResource getAggregatorResource(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<AggregatorResource> listAggregatorResource(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<AggregatorResource> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<AggregatorResource> list);
    
}
