package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiInfo;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:02
 */
public interface ApiInfoMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiInfo selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiInfo
     */
    void insert(ApiInfo apiInfo);

    /**
     * 更新数据库记录
     *
     * @param apiInfo
     */
    void update(ApiInfo apiInfo);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiInfo getApiInfo(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiInfo> listApiInfo(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<ApiInfo> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiInfo> list);
    
}
