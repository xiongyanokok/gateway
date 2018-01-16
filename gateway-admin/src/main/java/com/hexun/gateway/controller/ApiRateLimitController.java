package com.hexun.gateway.controller;

import java.util.Date;
import java.util.HashMap;
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
import com.hexun.gateway.model.ApiRateLimit;
import com.hexun.gateway.service.ApiRateLimitService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:10
 */
@Controller
@RequestMapping(value = "/admin/apiratelimit", produces = { "application/json; charset=UTF-8" })
public class ApiRateLimitController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ApiRateLimitController.class);

    @Autowired
	private ApiRateLimitService apiRateLimitService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "apiratelimit/list";
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
			return apiRateLimitService.listApiRateLimit(map);
		});
	}
	
	/**
	 * 进入限流页面
	 * 
	 * @param model
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/ratelimit", method = { RequestMethod.GET })
	public String ratelimit(Model model, Integer apiId) {
		Assert.notNull(apiId, "apiId为空");
		model.addAttribute("apiId", apiId);
		Map<String, Object> map = new HashMap<>();
		map.put("apiId", apiId);
		ApiRateLimit apiRateLimit = apiRateLimitService.getApiRateLimit(map);
		if (null == apiRateLimit) {
			return "apiratelimit/add";
		} else {
			model.addAttribute("apiRateLimit", apiRateLimit);
			return "apiratelimit/edit";
		}
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model) {
		return "apiratelimit/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiRateLimit
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiRateLimit apiRateLimit) {
		Assert.notNull(apiRateLimit, "保存数据为空");
		apiRateLimit.setCreateUserId(getUserId());
		apiRateLimit.setCreateTime(new Date());
		apiRateLimit.setUpdateUserId(getUserId());
		apiRateLimit.setUpdateTime(new Date());
		apiRateLimit.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiRateLimitService.save(apiRateLimit);
		logger.info("【{}】保存成功", apiRateLimit);
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
		ApiRateLimit apiRateLimit = apiRateLimitService.getApiRateLimitById(id);
		Assert.notNull(apiRateLimit, "数据不存在");
		model.addAttribute("apiRateLimit", apiRateLimit);
		return "apiratelimit/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiRateLimit
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiRateLimit apiRateLimit) {
		Assert.notNull(apiRateLimit, "修改数据为空");
		ApiRateLimit apiRateLimitInfo = apiRateLimitService.getApiRateLimitById(apiRateLimit.getId());
		Assert.notNull(apiRateLimitInfo, "数据不存在");
		apiRateLimit.setUpdateUserId(getUserId());
		apiRateLimit.setUpdateTime(new Date());
		apiRateLimitService.update(apiRateLimit);
		logger.info("【{}】修改成功", apiRateLimit);
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
		ApiRateLimit apiRateLimit = apiRateLimitService.getApiRateLimitById(id);
		Assert.notNull(apiRateLimit, "数据不存在");
		apiRateLimitService.remove(apiRateLimit);
		logger.info("【{}】删除成功", apiRateLimit);
		return buildSuccess("删除成功");
	}
	
}
