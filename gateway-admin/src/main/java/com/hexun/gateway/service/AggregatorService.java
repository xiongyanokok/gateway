package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.Aggregator;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:40
 */
public interface AggregatorService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Aggregator selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    Aggregator getAggregatorById(Integer id);
    
    /**
     * 保存数据
     *
     * @param aggregator
     */
    void save(Aggregator aggregator);

    /**
     * 修改数据
     *
     * @param aggregator
     */
    void update(Aggregator aggregator);
    
    /**
     * 删除数据
     * 
     * @param aggregator
     */
    void remove(Aggregator aggregator);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Aggregator getAggregator(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Aggregator> listAggregator(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<Aggregator> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Aggregator> list);
    
}
