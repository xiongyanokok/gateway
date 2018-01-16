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
import com.hexun.gateway.mapper.ApiProjectMapper;
import com.hexun.gateway.model.ApiProject;
import com.hexun.gateway.service.ApiProjectService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:07
 */
@Service
public class ApiProjectServiceImpl implements ApiProjectService {

    @Autowired
	private ApiProjectMapper apiProjectMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public ApiProject selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return apiProjectMapper.selectByPrimaryKey(id);
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
    public ApiProject getApiProjectById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>();
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return apiProjectMapper.getApiProject(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param apiProject
     * @throws GatewayException
     */
    @Override
    public void save(ApiProject apiProject) {
    	Assert.notNull(apiProject, "保存数据为空");
    	try {
			apiProjectMapper.insert(apiProject);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + apiProject.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param apiProject
     * @throws GatewayException
     */
    @Override
    public void update(ApiProject apiProject) {
    	Assert.notNull(apiProject, "修改数据为空");
    	try {
    		apiProjectMapper.update(apiProject);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + apiProject.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param apiProject
     * @throws GatewayException
     */
    @Override
    public void remove(ApiProject apiProject) {
    	Assert.notNull(apiProject, "删除数据为空");
		try {
    		ApiProject deleteApiProject = new ApiProject();
    		deleteApiProject.setId(apiProject.getId());
    		deleteApiProject.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		apiProjectMapper.update(deleteApiProject);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + apiProject.toString() + "】删除失败", e);
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
    public ApiProject getApiProject(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiProjectMapper.getApiProject(map);
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
    public List<ApiProject> listApiProject(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return apiProjectMapper.listApiProject(map);
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
    public void batchSave(List<ApiProject> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ApiProject>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiProject> page : pageList) {
				apiProjectMapper.batchInsert(page);
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
    public void batchUpdate(List<ApiProject> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ApiProject>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<ApiProject> page : pageList) {
				apiProjectMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
