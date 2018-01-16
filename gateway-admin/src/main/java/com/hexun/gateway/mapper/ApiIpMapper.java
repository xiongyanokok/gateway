package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiIp;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:04
 */
public interface ApiIpMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiIp selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiIp
     */
    void insert(ApiIp apiIp);

    /**
     * 更新数据库记录
     *
     * @param apiIp
     */
    void update(ApiIp apiIp);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiIp getApiIp(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiIp> listApiIp(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<ApiIp> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiIp> list);
    
}
