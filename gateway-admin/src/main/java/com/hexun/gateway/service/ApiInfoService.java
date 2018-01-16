package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiInfo;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:02
 */
public interface ApiInfoService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiInfo selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiInfo getApiInfoById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiInfo
     */
    void save(ApiInfo apiInfo);

    /**
     * 修改数据
     *
     * @param apiInfo
     */
    void update(ApiInfo apiInfo);
    
    /**
     * 删除数据
     * 
     * @param apiInfo
     */
    void remove(ApiInfo apiInfo);
    
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
    void batchSave(List<ApiInfo> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiInfo> list);
    
}
