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
import com.hexun.gateway.mapper.AggregatorMapper;
import com.hexun.gateway.model.Aggregator;
import com.hexun.gateway.service.AggregatorService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:40
 */
@Service
public class AggregatorServiceImpl implements AggregatorService {

    @Autowired
	private AggregatorMapper aggregatorMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws GatewayException
     */
    @Override
    public Aggregator selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return aggregatorMapper.selectByPrimaryKey(id);
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
    public Aggregator getAggregatorById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return aggregatorMapper.getAggregator(map);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param aggregator
     * @throws GatewayException
     */
    @Override
    public void save(Aggregator aggregator) {
    	Assert.notNull(aggregator, "保存数据为空");
    	try {
			aggregatorMapper.insert(aggregator);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + aggregator.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param aggregator
     * @throws GatewayException
     */
    @Override
    public void update(Aggregator aggregator) {
    	Assert.notNull(aggregator, "修改数据为空");
    	try {
    		aggregatorMapper.update(aggregator);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + aggregator.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param aggregator
     * @throws GatewayException
     */
    @Override
    public void remove(Aggregator aggregator) {
    	Assert.notNull(aggregator, "删除数据为空");
		try {
    		Aggregator deleteAggregator = new Aggregator();
    		deleteAggregator.setId(aggregator.getId());
    		deleteAggregator.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		aggregatorMapper.update(deleteAggregator);
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + aggregator.toString() + "】删除失败", e);
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
    public Aggregator getAggregator(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return aggregatorMapper.getAggregator(map);
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
    public List<Aggregator> listAggregator(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return aggregatorMapper.listAggregator(map);
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
    public void batchSave(List<Aggregator> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<Aggregator>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<Aggregator> page : pageList) {
				aggregatorMapper.batchInsert(page);
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
    public void batchUpdate(List<Aggregator> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<Aggregator>> pageList = ListPageUtil.listPage(list, 1000);
			for (List<Aggregator> page : pageList) {
				aggregatorMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new GatewayException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
