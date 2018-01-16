package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiProject;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:07
 */
public interface ApiProjectService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiProject selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiProject getApiProjectById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiProject
     */
    void save(ApiProject apiProject);

    /**
     * 修改数据
     *
     * @param apiProject
     */
    void update(ApiProject apiProject);
    
    /**
     * 删除数据
     * 
     * @param apiProject
     */
    void remove(ApiProject apiProject);
    
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
    void batchSave(List<ApiProject> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiProject> list);
    
}
