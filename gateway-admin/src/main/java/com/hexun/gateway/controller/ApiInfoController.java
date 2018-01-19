package com.hexun.gateway.controller;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.hexun.common.utils.DateUtils;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.common.Assert;
import com.hexun.gateway.enums.TrueFalseStatusEnum;
import com.hexun.gateway.model.ApiInfo;
import com.hexun.gateway.model.ApiProject;
import com.hexun.gateway.service.ApiInfoService;
import com.hexun.gateway.service.ApiProjectService;
import com.hexun.gateway.zookeeper.RegistryCenter;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:02
 */
@Controller
@RequestMapping(value = "/admin/apiinfo", produces = { "application/json; charset=UTF-8" })
public class ApiInfoController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ApiInfoController.class);

    @Autowired
	private ApiInfoService apiInfoService;
    
    @Autowired
    private ApiProjectService apiProjectService;
    
    @Autowired
	private RegistryCenter registryCenter; 
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		Map<String, Object> map = new HashMap<>();
		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
		List<ApiProject> apiProjects = apiProjectService.listApiProject(map);
		model.addAttribute("apiProjects", apiProjects);
		model.addAttribute("apiProjectsJson", JsonUtils.obj2String(apiProjects));
		return "apiinfo/list";
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
			map.put("projectId", request.getParameter("projectId"));
			return apiInfoService.listApiInfo(map);
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
		Map<String, Object> map = new HashMap<>();
		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
		List<ApiProject> apiProjects = apiProjectService.listApiProject(map);
		model.addAttribute("apiProjects", apiProjects);
		return "apiinfo/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiInfo
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiInfo apiInfo) {
		Assert.notNull(apiInfo, "保存数据为空");
		apiInfo.setCreateUserId(getUserId());
		apiInfo.setCreateTime(new Date());
		apiInfo.setUpdateUserId(getUserId());
		apiInfo.setUpdateTime(new Date());
		apiInfo.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiInfoService.save(apiInfo);
		logger.info("【{}】保存成功", apiInfo);
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
		ApiInfo apiInfo = apiInfoService.getApiInfoById(id);
		Assert.notNull(apiInfo, "数据不存在");
		model.addAttribute("apiInfo", apiInfo);
		Map<String, Object> map = new HashMap<>();
		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
		List<ApiProject> apiProjects = apiProjectService.listApiProject(map);
		model.addAttribute("apiProjects", apiProjects);
		return "apiinfo/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiInfo
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiInfo apiInfo) {
		Assert.notNull(apiInfo, "修改数据为空");
		ApiInfo apiInfoInfo = apiInfoService.getApiInfoById(apiInfo.getId());
		Assert.notNull(apiInfoInfo, "数据不存在");
		apiInfo.setUpdateUserId(getUserId());
		apiInfo.setUpdateTime(new Date());
		apiInfoService.update(apiInfo);
		logger.info("【{}】修改成功", apiInfo);
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
		ApiInfo apiInfo = apiInfoService.getApiInfoById(id);
		Assert.notNull(apiInfo, "数据不存在");
		apiInfoService.remove(apiInfo);
		logger.info("【{}】删除成功", apiInfo);
		return buildSuccess("删除成功");
	}
	
	/**
	 * 刷新数据
	 * 
	 * @return
	 */
	@RequestMapping(value = "/refresh", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> refresh() {
		registryCenter.update("/api", DateUtils.now());
		return buildSuccess("刷新成功");
	}
	
}
