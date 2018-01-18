package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.AggregatorResource;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
public interface AggregatorResourceService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    AggregatorResource selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    AggregatorResource getAggregatorResourceById(Integer id);
    
    /**
     * 保存数据
     *
     * @param aggregatorResource
     */
    void save(AggregatorResource aggregatorResource);

    /**
     * 修改数据
     *
     * @param aggregatorResource
     */
    void update(AggregatorResource aggregatorResource);
    
    /**
     * 删除数据
     * 
     * @param aggregatorResource
     */
    void remove(AggregatorResource aggregatorResource);
    
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
    void batchSave(List<AggregatorResource> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<AggregatorResource> list);
    
}
