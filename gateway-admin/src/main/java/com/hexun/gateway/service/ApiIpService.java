package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiIp;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:04
 */
public interface ApiIpService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiIp selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiIp getApiIpById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiIp
     */
    void save(ApiIp apiIp);

    /**
     * 修改数据
     *
     * @param apiIp
     */
    void update(ApiIp apiIp);
    
    /**
     * 删除数据
     * 
     * @param apiIp
     */
    void remove(ApiIp apiIp);
    
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
    void batchSave(List<ApiIp> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiIp> list);
    
}
