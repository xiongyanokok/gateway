package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiSign;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
public interface ApiSignMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiSign selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiSign
     */
    void insert(ApiSign apiSign);

    /**
     * 更新数据库记录
     *
     * @param apiSign
     */
    void update(ApiSign apiSign);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiSign getApiSign(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiSign> listApiSign(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<ApiSign> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiSign> list);
    
}
