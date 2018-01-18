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
import com.hexun.gateway.model.Aggregator;
import com.hexun.gateway.service.AggregatorService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月18日 上午10:40:40
 */
@Controller
@RequestMapping(value = "/admin/aggregator", produces = { "application/json; charset=UTF-8" })
public class AggregatorController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(AggregatorController.class);

    @Autowired
	private AggregatorService aggregatorService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "aggregator/list";
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
			return aggregatorService.listAggregator(map);
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
		return "aggregator/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param aggregator
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Aggregator aggregator) {
		Assert.notNull(aggregator, "保存数据为空");
		aggregator.setCreateUserId(getUserId());
		aggregator.setCreateTime(new Date());
		aggregator.setUpdateUserId(getUserId());
		aggregator.setUpdateTime(new Date());
		aggregator.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		aggregatorService.save(aggregator);
		logger.info("【{}】保存成功", aggregator);
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
		Aggregator aggregator = aggregatorService.getAggregatorById(id);
		Assert.notNull(aggregator, "数据不存在");
		model.addAttribute("aggregator", aggregator);
		return "aggregator/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param aggregator
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Aggregator aggregator) {
		Assert.notNull(aggregator, "修改数据为空");
		Aggregator aggregatorInfo = aggregatorService.getAggregatorById(aggregator.getId());
		Assert.notNull(aggregatorInfo, "数据不存在");
		aggregator.setUpdateUserId(getUserId());
		aggregator.setUpdateTime(new Date());
		aggregatorService.update(aggregator);
		logger.info("【{}】修改成功", aggregator);
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
		Aggregator aggregator = aggregatorService.getAggregatorById(id);
		Assert.notNull(aggregator, "数据不存在");
		aggregatorService.remove(aggregator);
		logger.info("【{}】删除成功", aggregator);
		return buildSuccess("删除成功");
	}
	
}
