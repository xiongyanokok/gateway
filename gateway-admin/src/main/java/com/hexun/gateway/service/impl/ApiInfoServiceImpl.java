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
import com.hexun.gateway.mapper.ApiInfoMapper;
import com.hexun.gateway.model.ApiInfo;
import com.hexun.gateway.service.ApiInfoService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:02
 */
@Service
public class ApiInfoServiceImpl implements ApiInfoService {

    @Autowired
	private ApiInfoMapper apiInfoMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiInfo selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiInfoMapper.selectByPrimaryKey(id);
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
    public ApiInfo getApiInfoById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return apiInfoMapper.getApiInfo(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiInfo
     * @throws GatewayException
     */
    @Override
    public void save(ApiInfo apiInfo) {
    	Assert.notNull(apiInfo, "保存数据为空");
    	try {
			apiInfoMapper.insert(apiInfo);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiInfo.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiInfo
     * @throws GatewayException
     */
    @Override
    public void update(ApiInfo apiInfo) {
    	Assert.notNull(apiInfo, "修改数据为空");
    	try {
    		apiInfoMapper.update(apiInfo);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiInfo.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiInfo
     * @throws GatewayException
     */
    @Override
    public void remove(ApiInfo apiInfo) {
    	Assert.notNull(apiInfo, "删除数据为空");
		try {
    		ApiInfo deleteApiInfo = new ApiInfo();
    		deleteApiInfo.setId(apiInfo.getId());
    		deleteApiInfo.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiInfoMapper.update(deleteApiInfo);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiInfo.toString() + "】删除失败", e);
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
    public ApiInfo getApiInfo(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiInfoMapper.getApiInfo(map);
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
    public List<ApiInfo> listApiInfo(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiInfoMapper.listApiInfo(map);
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
    public void batchSave(List<ApiInfo> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiInfo>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiInfo> page : pageList) {
				apiInfoMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiInfo> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiInfo>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiInfo> page : pageList) {
				apiInfoMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
