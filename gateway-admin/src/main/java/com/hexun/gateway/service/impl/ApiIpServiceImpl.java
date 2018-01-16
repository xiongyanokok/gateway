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
import com.hexun.gateway.mapper.ApiIpMapper;
import com.hexun.gateway.model.ApiIp;
import com.hexun.gateway.service.ApiIpService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:04
 */
@Service
public class ApiIpServiceImpl implements ApiIpService {

    @Autowired
	private ApiIpMapper apiIpMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiIp selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiIpMapper.selectByPrimaryKey(id);
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
    public ApiIp getApiIpById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return apiIpMapper.getApiIp(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiIp
     * @throws GatewayException
     */
    @Override
    public void save(ApiIp apiIp) {
    	Assert.notNull(apiIp, "保存数据为空");
    	try {
			apiIpMapper.insert(apiIp);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiIp.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiIp
     * @throws GatewayException
     */
    @Override
    public void update(ApiIp apiIp) {
    	Assert.notNull(apiIp, "修改数据为空");
    	try {
    		apiIpMapper.update(apiIp);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiIp.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiIp
     * @throws GatewayException
     */
    @Override
    public void remove(ApiIp apiIp) {
    	Assert.notNull(apiIp, "删除数据为空");
		try {
    		ApiIp deleteApiIp = new ApiIp();
    		deleteApiIp.setId(apiIp.getId());
    		deleteApiIp.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiIpMapper.update(deleteApiIp);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiIp.toString() + "】删除失败", e);
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
    public ApiIp getApiIp(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiIpMapper.getApiIp(map);
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
    public List<ApiIp> listApiIp(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiIpMapper.listApiIp(map);
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
    public void batchSave(List<ApiIp> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiIp>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiIp> page : pageList) {
				apiIpMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiIp> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiIp>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiIp> page : pageList) {
				apiIpMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
