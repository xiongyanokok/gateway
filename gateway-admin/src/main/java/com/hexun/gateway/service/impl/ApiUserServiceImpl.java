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
import com.hexun.gateway.mapper.ApiUserMapper;
import com.hexun.gateway.model.ApiUser;
import com.hexun.gateway.service.ApiUserService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:15
 */
@Service
public class ApiUserServiceImpl implements ApiUserService {

    @Autowired
	private ApiUserMapper apiUserMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiUser selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiUserMapper.selectByPrimaryKey(id);
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
    public ApiUser getApiUserById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return apiUserMapper.getApiUser(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiUser
     * @throws GatewayException
     */
    @Override
    public void save(ApiUser apiUser) {
    	Assert.notNull(apiUser, "保存数据为空");
    	try {
			apiUserMapper.insert(apiUser);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiUser.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiUser
     * @throws GatewayException
     */
    @Override
    public void update(ApiUser apiUser) {
    	Assert.notNull(apiUser, "修改数据为空");
    	try {
    		apiUserMapper.update(apiUser);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiUser.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiUser
     * @throws GatewayException
     */
    @Override
    public void remove(ApiUser apiUser) {
    	Assert.notNull(apiUser, "删除数据为空");
		try {
    		ApiUser deleteApiUser = new ApiUser();
    		deleteApiUser.setId(apiUser.getId());
    		deleteApiUser.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiUserMapper.update(deleteApiUser);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiUser.toString() + "】删除失败", e);
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
    public ApiUser getApiUser(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiUserMapper.getApiUser(map);
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
    public List<ApiUser> listApiUser(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiUserMapper.listApiUser(map);
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
    public void batchSave(List<ApiUser> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiUser>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiUser> page : pageList) {
				apiUserMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiUser> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiUser>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiUser> page : pageList) {
				apiUserMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
