package com.hexun.gateway.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.hexun.common.utils.ListPageUtil;
import com.hexun.gateway.common.Assert;
import com.hexun.gateway.enums.ErrorCodeEnum;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.exception.GatewayException;
import com.hexun.gateway.mapper.AggregatorResourceMapper;
import com.hexun.gateway.model.AggregatorResource;
import com.hexun.gateway.service.AggregatorResourceService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
@Service
public class AggregatorResourceServiceImpl implements AggregatorResourceService {

    @Autowired
	private AggregatorResourceMapper aggregatorResourceMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public AggregatorResource selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return aggregatorResourceMapper.selectByPrimaryKey(id);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public AggregatorResource getAggregatorResourceById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return aggregatorResourceMapper.getAggregatorResource(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param aggregatorResource
     * @throws GatewayException
     */
    @Override
    public void save(AggregatorResource aggregatorResource) {
    	Assert.notNull(aggregatorResource, "保存数据为空");
    	try {
			aggregatorResourceMapper.insert(aggregatorResource);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + aggregatorResource.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param aggregatorResource
     * @throws GatewayException
     */
    @Override
    public void update(AggregatorResource aggregatorResource) {
    	Assert.notNull(aggregatorResource, "修改数据为空");
    	try {
    		aggregatorResourceMapper.update(aggregatorResource);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + aggregatorResource.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param aggregatorResource
     * @throws GatewayException
     */
    @Override
    public void remove(AggregatorResource aggregatorResource) {
    	Assert.notNull(aggregatorResource, "删除数据为空");
		try {
    		AggregatorResource deleteAggregatorResource = new AggregatorResource();
    		deleteAggregatorResource.setId(aggregatorResource.getId());
    		deleteAggregatorResource.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		aggregatorResourceMapper.update(deleteAggregatorResource);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + aggregatorResource.toString() + "】删除失败", e);
    	}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws GatewayException
     */
    @Override
    public AggregatorResource getAggregatorResource(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return aggregatorResourceMapper.getAggregatorResource(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询对象失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws GatewayException
     */
    @Override
    public List<AggregatorResource> listAggregatorResource(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return aggregatorResourceMapper.listAggregatorResource(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询列表失败", e);
		}
    }
    
    /**
     * 批量保存
     * 
     * @param list
     * @throws GatewayException
     */
    @Override
    public void batchSave(List<AggregatorResource> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<AggregatorResource>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<AggregatorResource> page : pageList) {
				aggregatorResourceMapper.batchInsert(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量保存失败", e);
		}
    }
    
    /**
     * 批量更新
     * 
     * @param list
     * @throws GatewayException
     */
    @Override
    public void batchUpdate(List<AggregatorResource> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<AggregatorResource>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<AggregatorResource> page : pageList) {
				aggregatorResourceMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
