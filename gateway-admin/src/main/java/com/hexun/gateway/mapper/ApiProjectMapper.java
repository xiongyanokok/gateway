package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiProject;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:07
 */
public interface ApiProjectMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiProject selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiProject
     */
    void insert(ApiProject apiProject);

    /**
     * 更新数据库记录
     *
     * @param apiProject
     */
    void update(ApiProject apiProject);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiProject getApiProject(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiProject> listApiProject(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<ApiProject> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiProject> list);
    
}
