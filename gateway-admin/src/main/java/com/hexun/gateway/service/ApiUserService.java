package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiUser;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:15
 */
public interface ApiUserService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiUser selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiUser getApiUserById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiUser
     */
    void save(ApiUser apiUser);

    /**
     * 修改数据
     *
     * @param apiUser
     */
    void update(ApiUser apiUser);
    
    /**
     * 删除数据
     * 
     * @param apiUser
     */
    void remove(ApiUser apiUser);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ApiUser getApiUser(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ApiUser> listApiUser(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ApiUser> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiUser> list);
    
}
