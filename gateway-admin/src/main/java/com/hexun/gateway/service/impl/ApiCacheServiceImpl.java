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
import com.hexun.gateway.mapper.ApiCacheMapper;
import com.hexun.gateway.model.ApiCache;
import com.hexun.gateway.service.ApiCacheService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:58:58
 */
@Service
public class ApiCacheServiceImpl implements ApiCacheService {

    @Autowired
	private ApiCacheMapper apiCacheMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiCache selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiCacheMapper.selectByPrimaryKey(id);
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
    public ApiCache getApiCacheById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return apiCacheMapper.getApiCache(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiCache
     * @throws GatewayException
     */
    @Override
    public void save(ApiCache apiCache) {
    	Assert.notNull(apiCache, "保存数据为空");
    	try {
			apiCacheMapper.insert(apiCache);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiCache.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiCache
     * @throws GatewayException
     */
    @Override
    public void update(ApiCache apiCache) {
    	Assert.notNull(apiCache, "修改数据为空");
    	try {
    		apiCacheMapper.update(apiCache);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiCache.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiCache
     * @throws GatewayException
     */
    @Override
    public void remove(ApiCache apiCache) {
    	Assert.notNull(apiCache, "删除数据为空");
		try {
    		ApiCache deleteApiCache = new ApiCache();
    		deleteApiCache.setId(apiCache.getId());
    		deleteApiCache.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiCacheMapper.update(deleteApiCache);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiCache.toString() + "】删除失败", e);
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
    public ApiCache getApiCache(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiCacheMapper.getApiCache(map);
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
    public List<ApiCache> listApiCache(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiCacheMapper.listApiCache(map);
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
    public void batchSave(List<ApiCache> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiCache>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiCache> page : pageList) {
				apiCacheMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiCache> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiCache>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiCache> page : pageList) {
				apiCacheMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
