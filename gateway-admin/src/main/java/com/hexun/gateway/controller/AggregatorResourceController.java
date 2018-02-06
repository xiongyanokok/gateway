package com.hexun.gateway.controller;

import java.util.Date;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.hexun.gateway.common.Assert;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.model.Aggregator;
import com.hexun.gateway.model.AggregatorResource;
import com.hexun.gateway.service.AggregatorResourceService;
import com.hexun.gateway.service.AggregatorService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:45
 */
@Controller
@RequestMapping(value = "/admin/aggregatorresource", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class AggregatorResourceController extends BaseController {

    @Autowired
	private AggregatorResourceService aggregatorResourceService;
    
    @Autowired
    private AggregatorService aggregatorService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model, Integer aggregatorId) {
		Assert.notNull(aggregatorId, "aggregatorId为空");
		model.addAttribute("aggregatorId", aggregatorId);
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
			map.put("aggregatorId", request.getParameter("aggregatorId"));
			map.put("resourceName", request.getParameter("resourceName"));
			map.put("resourceUrl", request.getParameter("resourceUrl"));
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
	public String add(Model model, Integer aggregatorId) {
		Assert.notNull(aggregatorId, "aggregatorId为空");
		Aggregator aggregator = aggregatorService.getAggregatorById(aggregatorId);
		Assert.notNull(aggregator, "数据不存在");
		model.addAttribute("aggregatorId", aggregatorId);
		model.addAttribute("aggregatorType", aggregator.getType());
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
		log.info("【{}】保存成功", aggregatorResource);
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
		
		Aggregator aggregator = aggregatorService.getAggregatorById(aggregatorResource.getAggregatorId());
		Assert.notNull(aggregator, "数据不存在");
		model.addAttribute("aggregatorType", aggregator.getType());
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
		log.info("【{}】修改成功", aggregatorResource);
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
		log.info("【{}】删除成功", aggregatorResource);
		return buildSuccess("删除成功");
	}
	
}
