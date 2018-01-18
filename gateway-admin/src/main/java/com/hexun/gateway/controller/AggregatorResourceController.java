package com.hexun.gateway.controller;

import java.util.Date;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.hexun.gateway.common.Assert;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.model.AggregatorResource;
import com.hexun.gateway.service.AggregatorResourceService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
@Controller
@RequestMapping(value = "/admin/aggregatorresource", produces = { "application/json; charset=UTF-8" })
public class AggregatorResourceController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(AggregatorResourceController.class);

    @Autowired
	private AggregatorResourceService aggregatorResourceService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "aggregatorresource/list";
	}
	
	/**
	 * 列表分页查询
	 * 
	 * @return
	 */
	@RequestMapping(value = "/query", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> query() {
		return pageInfoResult(map -> {
			// 查询条件
			return aggregatorResourceService.listAggregatorResource(map);
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model) {
		return "aggregatorresource/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param aggregatorResource
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(AggregatorResource aggregatorResource) {
		Assert.notNull(aggregatorResource, "保存数据为空");
		aggregatorResource.setCreateUserId(getUserId());
		aggregatorResource.setCreateTime(new Date());
		aggregatorResource.setUpdateUserId(getUserId());
		aggregatorResource.setUpdateTime(new Date());
		aggregatorResource.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		aggregatorResourceService.save(aggregatorResource);
		logger.info("【{}】保存成功", aggregatorResource);
		return buildSuccess("保存成功");
	}
	
	/**
	 * 进入修改页面
	 * 
	 * @param model
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/edit", method = { RequestMethod.GET })
	public String edit(Model model, Integer id) {
		Assert.notNull(id, "id为空");
		AggregatorResource aggregatorResource = aggregatorResourceService.getAggregatorResourceById(id);
		Assert.notNull(aggregatorResource, "数据不存在");
		model.addAttribute("aggregatorResource", aggregatorResource);
		return "aggregatorresource/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param aggregatorResource
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(AggregatorResource aggregatorResource) {
		Assert.notNull(aggregatorResource, "修改数据为空");
		AggregatorResource aggregatorResourceInfo = aggregatorResourceService.getAggregatorResourceById(aggregatorResource.getId());
		Assert.notNull(aggregatorResourceInfo, "数据不存在");
		aggregatorResource.setUpdateUserId(getUserId());
		aggregatorResource.setUpdateTime(new Date());
		aggregatorResourceService.update(aggregatorResource);
		logger.info("【{}】修改成功", aggregatorResource);
		return buildSuccess("修改成功");
	}
	
	/**
	 * 删除数据
	 * 
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/delete", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> delete(Integer id) {
		Assert.notNull(id, "id为空");
		AggregatorResource aggregatorResource = aggregatorResourceService.getAggregatorResourceById(id);
		Assert.notNull(aggregatorResource, "数据不存在");
		aggregatorResourceService.remove(aggregatorResource);
		logger.info("【{}】删除成功", aggregatorResource);
		return buildSuccess("删除成功");
	}
	
}
