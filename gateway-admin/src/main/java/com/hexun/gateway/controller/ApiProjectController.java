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
import com.hexun.gateway.model.ApiProject;
import com.hexun.gateway.service.ApiProjectService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:07
 */
@Controller
@RequestMapping(value = "/admin/apiproject", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class ApiProjectController extends BaseController {

    @Autowired
	private ApiProjectService apiProjectService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "apiproject/list";
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
			return apiProjectService.listApiProject(map);
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
		return "apiproject/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiProject
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiProject apiProject) {
		Assert.notNull(apiProject, "保存数据为空");
		apiProject.setCreateUserId(getUserId());
		apiProject.setCreateTime(new Date());
		apiProject.setUpdateUserId(getUserId());
		apiProject.setUpdateTime(new Date());
		apiProject.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiProjectService.save(apiProject);
		log.info("【{}】保存成功", apiProject);
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
		ApiProject apiProject = apiProjectService.getApiProjectById(id);
		Assert.notNull(apiProject, "数据不存在");
		model.addAttribute("apiProject", apiProject);
		return "apiproject/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiProject
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiProject apiProject) {
		Assert.notNull(apiProject, "修改数据为空");
		ApiProject apiProjectInfo = apiProjectService.getApiProjectById(apiProject.getId());
		Assert.notNull(apiProjectInfo, "数据不存在");
		apiProject.setUpdateUserId(getUserId());
		apiProject.setUpdateTime(new Date());
		apiProjectService.update(apiProject);
		log.info("【{}】修改成功", apiProject);
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
		ApiProject apiProject = apiProjectService.getApiProjectById(id);
		Assert.notNull(apiProject, "数据不存在");
		apiProjectService.remove(apiProject);
		log.info("【{}】删除成功", apiProject);
		return buildSuccess("删除成功");
	}
	
}
