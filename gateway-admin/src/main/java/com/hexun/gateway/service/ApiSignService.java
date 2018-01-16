package com.hexun.gateway.service;

import java.util.List;
import java.util.Map;

import com.hexun.gateway.model.ApiSign;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
public interface ApiSignService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ApiSign selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ApiSign getApiSignById(Integer id);
    
    /**
     * 保存数据
     *
     * @param apiSign
     */
    void save(ApiSign apiSign);

    /**
     * 修改数据
     *
     * @param apiSign
     */
    void update(ApiSign apiSign);
    
    /**
     * 删除数据
     * 
     * @param apiSign
     */
    void remove(ApiSign apiSign);
    
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
    void batchSave(List<ApiSign> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ApiSign> list);
    
}
