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
import com.hexun.gateway.model.ApiUser;
import com.hexun.gateway.service.ApiUserService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:15
 */
@Controller
@RequestMapping(value = "/admin/apiuser", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class ApiUserController extends BaseController {

    @Autowired
	private ApiUserService apiUserService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model, Integer apiId) {
		model.addAttribute("apiId", apiId);
		return "apiuser/list";
	}
	
	/**
	 * 列表分页查询
	 * 
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/query", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> query(Integer apiId) {
		return pageInfoResult(map -> {
			// 查询条件
			map.put("apiId", apiId);
			map.put("userId", request.getParameter("userId"));
			return apiUserService.listApiUser(map);
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model, Integer apiId) {
		model.addAttribute("apiId", apiId);
		return "apiuser/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiUser
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiUser apiUser) {
		Assert.notNull(apiUser, "保存数据为空");
		apiUser.setCreateUserId(getUserId());
		apiUser.setCreateTime(new Date());
		apiUser.setUpdateUserId(getUserId());
		apiUser.setUpdateTime(new Date());
		apiUser.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiUserService.save(apiUser);
		log.info("【{}】保存成功", apiUser);
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
		ApiUser apiUser = apiUserService.getApiUserById(id);
		Assert.notNull(apiUser, "数据不存在");
		model.addAttribute("apiUser", apiUser);
		return "apiuser/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiUser
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiUser apiUser) {
		Assert.notNull(apiUser, "修改数据为空");
		ApiUser apiUserInfo = apiUserService.getApiUserById(apiUser.getId());
		Assert.notNull(apiUserInfo, "数据不存在");
		apiUser.setUpdateUserId(getUserId());
		apiUser.setUpdateTime(new Date());
		apiUserService.update(apiUser);
		log.info("【{}】修改成功", apiUser);
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
		ApiUser apiUser = apiUserService.getApiUserById(id);
		Assert.notNull(apiUser, "数据不存在");
		apiUserService.remove(apiUser);
		log.info("【{}】删除成功", apiUser);
		return buildSuccess("删除成功");
	}
	
}
