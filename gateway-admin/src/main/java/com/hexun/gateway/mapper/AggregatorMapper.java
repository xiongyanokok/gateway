package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.Aggregator;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:40
 */
public interface AggregatorMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Aggregator selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param aggregator
     */
    void insert(Aggregator aggregator);

    /**
     * 更新数据库记录
     *
     * @param aggregator
     */
    void update(Aggregator aggregator);

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
    void batchInsert(List<Aggregator> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Aggregator> list);
    
}
