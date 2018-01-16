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
import com.hexun.gateway.mapper.ApiSignMapper;
import com.hexun.gateway.model.ApiSign;
import com.hexun.gateway.service.ApiSignService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
@Service
public class ApiSignServiceImpl implements ApiSignService {

    @Autowired
	private ApiSignMapper apiSignMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiSign selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiSignMapper.selectByPrimaryKey(id);
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
    public ApiSign getApiSignById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return apiSignMapper.getApiSign(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiSign
     * @throws GatewayException
     */
    @Override
    public void save(ApiSign apiSign) {
    	Assert.notNull(apiSign, "保存数据为空");
    	try {
			apiSignMapper.insert(apiSign);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiSign.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiSign
     * @throws GatewayException
     */
    @Override
    public void update(ApiSign apiSign) {
    	Assert.notNull(apiSign, "修改数据为空");
    	try {
    		apiSignMapper.update(apiSign);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiSign.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiSign
     * @throws GatewayException
     */
    @Override
    public void remove(ApiSign apiSign) {
    	Assert.notNull(apiSign, "删除数据为空");
		try {
    		ApiSign deleteApiSign = new ApiSign();
    		deleteApiSign.setId(apiSign.getId());
    		deleteApiSign.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiSignMapper.update(deleteApiSign);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiSign.toString() + "】删除失败", e);
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
    public ApiSign getApiSign(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiSignMapper.getApiSign(map);
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
    public List<ApiSign> listApiSign(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiSignMapper.listApiSign(map);
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
    public void batchSave(List<ApiSign> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiSign>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiSign> page : pageList) {
				apiSignMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiSign> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiSign>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiSign> page : pageList) {
				apiSignMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
