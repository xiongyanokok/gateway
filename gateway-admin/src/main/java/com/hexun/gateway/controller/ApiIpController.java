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
import com.hexun.gateway.model.ApiIp;
import com.hexun.gateway.service.ApiIpService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:04
 */
@Controller
@RequestMapping(value = "/admin/apiip", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class ApiIpController extends BaseController {

    @Autowired
	private ApiIpService apiIpService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model, Integer apiId) {
		if (null == apiId) {
			
		}
		model.addAttribute("apiId", apiId);
		return "apiip/list";
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
			map.put("ip", request.getParameter("ip"));
			return apiIpService.listApiIp(map);
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
		return "apiip/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiIp
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiIp apiIp) {
		Assert.notNull(apiIp, "保存数据为空");
		apiIp.setCreateUserId(getUserId());
		apiIp.setCreateTime(new Date());
		apiIp.setUpdateUserId(getUserId());
		apiIp.setUpdateTime(new Date());
		apiIp.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiIpService.save(apiIp);
		log.info("【{}】保存成功", apiIp);
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
		ApiIp apiIp = apiIpService.getApiIpById(id);
		Assert.notNull(apiIp, "数据不存在");
		model.addAttribute("apiIp", apiIp);
		return "apiip/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiIp
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiIp apiIp) {
		Assert.notNull(apiIp, "修改数据为空");
		ApiIp apiIpInfo = apiIpService.getApiIpById(apiIp.getId());
		Assert.notNull(apiIpInfo, "数据不存在");
		apiIp.setUpdateUserId(getUserId());
		apiIp.setUpdateTime(new Date());
		apiIpService.update(apiIp);
		log.info("【{}】修改成功", apiIp);
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
		ApiIp apiIp = apiIpService.getApiIpById(id);
		Assert.notNull(apiIp, "数据不存在");
		apiIpService.remove(apiIp);
		log.info("【{}】删除成功", apiIp);
		return buildSuccess("删除成功");
	}
	
}
