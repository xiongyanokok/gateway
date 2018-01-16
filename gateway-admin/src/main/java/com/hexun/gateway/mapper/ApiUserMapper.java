package com.hexun.gateway.mapper;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiUser;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:15
 */
public interface ApiUserMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiUser selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param apiUser
     */
    void insert(ApiUser apiUser);

    /**
     * 更新数据库记录
     *
     * @param apiUser
     */
    void update(ApiUser apiUser);

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
    void batchInsert(List<ApiUser> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiUser> list);
    
}
