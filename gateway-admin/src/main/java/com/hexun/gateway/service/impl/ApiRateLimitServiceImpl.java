package com.hexun.gateway.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.hexun.common.utils.ListPageUtil;
import com.hexun.gateway.common.Assert;
import com.hexun.gateway.common.utils.CommonUtils;
import com.hexun.gateway.enums.ErrorCodeEnum;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.exception.GatewayException;
import com.hexun.gateway.mapper.ApiRateLimitMapper;
import com.hexun.gateway.model.ApiRateLimit;
import com.hexun.gateway.service.ApiRateLimitService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:10
 */
@Service
public class ApiRateLimitServiceImpl implements ApiRateLimitService {

    @Autowired
	private ApiRateLimitMapper apiRateLimitMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiRateLimit selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiRateLimitMapper.selectByPrimaryKey(id);
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
    public ApiRateLimit getApiRateLimitById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return apiRateLimitMapper.getApiRateLimit(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiRateLimit
     * @throws GatewayException
     */
    @Override
    public void save(ApiRateLimit apiRateLimit) {
    	Assert.notNull(apiRateLimit, "保存数据为空");
    	try {
			apiRateLimitMapper.insert(apiRateLimit);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiRateLimit.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiRateLimit
     * @throws GatewayException
     */
    @Override
    public void update(ApiRateLimit apiRateLimit) {
    	Assert.notNull(apiRateLimit, "修改数据为空");
    	try {
    		apiRateLimitMapper.update(apiRateLimit);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiRateLimit.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiRateLimit
     * @throws GatewayException
     */
    @Override
    public void remove(ApiRateLimit apiRateLimit) {
    	Assert.notNull(apiRateLimit, "删除数据为空");
		try {
    		ApiRateLimit deleteApiRateLimit = new ApiRateLimit();
    		deleteApiRateLimit.setId(apiRateLimit.getId());
    		deleteApiRateLimit.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiRateLimitMapper.update(deleteApiRateLimit);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiRateLimit.toString() + "】删除失败", e);
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
    public ApiRateLimit getApiRateLimit(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiRateLimitMapper.getApiRateLimit(map);
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
    public List<ApiRateLimit> listApiRateLimit(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiRateLimitMapper.listApiRateLimit(map);
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
    public void batchSave(List<ApiRateLimit> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiRateLimit>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiRateLimit> page : pageList) {
				apiRateLimitMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiRateLimit> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiRateLimit>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiRateLimit> page : pageList) {
				apiRateLimitMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
