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
import com.hexun.gateway.model.ApiCache;
import com.hexun.gateway.service.ApiCacheService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:58:58
 */
@Controller
@RequestMapping(value = "/admin/apicache", produces = { "application/json; charset=UTF-8" })
public class ApiCacheController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ApiCacheController.class);

    @Autowired
	private ApiCacheService apiCacheService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "apicache/list";
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
			return apiCacheService.listApiCache(map);
		});
	}
	
	/**
	 * 进入缓存页面
	 * 
	 * @param model
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/cache", method = { RequestMethod.GET })
	public String cache(Model model, Integer apiId) {
		Assert.notNull(apiId, "apiId为空");
		model.addAttribute("apiId", apiId);
		Map<String, Object> map = new HashMap<>();
		map.put("apiId", apiId);
		ApiCache apiCache = apiCacheService.getApiCache(map);
		if (null == apiCache) {
			return "apicache/add";
		} else {
			model.addAttribute("apiCache", apiCache);
			return "apicache/edit";
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
		return "apicache/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiCache
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiCache apiCache) {
		Assert.notNull(apiCache, "保存数据为空");
		apiCache.setCreateUserId(getUserId());
		apiCache.setCreateTime(new Date());
		apiCache.setUpdateUserId(getUserId());
		apiCache.setUpdateTime(new Date());
		apiCache.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiCacheService.save(apiCache);
		logger.info("【{}】保存成功", apiCache);
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
		ApiCache apiCache = apiCacheService.getApiCacheById(id);
		Assert.notNull(apiCache, "数据不存在");
		model.addAttribute("apiCache", apiCache);
		return "apicache/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiCache
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiCache apiCache) {
		Assert.notNull(apiCache, "修改数据为空");
		ApiCache apiCacheInfo = apiCacheService.getApiCacheById(apiCache.getId());
		Assert.notNull(apiCacheInfo, "数据不存在");
		apiCache.setUpdateUserId(getUserId());
		apiCache.setUpdateTime(new Date());
		apiCacheService.update(apiCache);
		logger.info("【{}】修改成功", apiCache);
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
		ApiCache apiCache = apiCacheService.getApiCacheById(id);
		Assert.notNull(apiCache, "数据不存在");
		apiCacheService.remove(apiCache);
		logger.info("【{}】删除成功", apiCache);
		return buildSuccess("删除成功");
	}
	
}
