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
import com.hexun.gateway.model.ApiSign;
import com.hexun.gateway.service.ApiSignService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月04日 下午03:59:12
 */
@Controller
@RequestMapping(value = "/admin/apisign", produces = { "application/json; charset=UTF-8" })
public class ApiSignController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ApiSignController.class);

    @Autowired
	private ApiSignService apiSignService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "apisign/list";
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
			return apiSignService.listApiSign(map);
		});
	}
	
	/**
	 * 进入鉴权页面
	 * 
	 * @param model
	 * @param apiId
	 * @return
	 */
	@RequestMapping(value = "/sign", method = { RequestMethod.GET })
	public String sign(Model model, Integer apiId) {
		Assert.notNull(apiId, "apiId为空");
		model.addAttribute("apiId", apiId);
		Map<String, Object> map = new HashMap<>();
		map.put("apiId", apiId);
		ApiSign apiSign = apiSignService.getApiSign(map);
		if (null == apiSign) {
			return "apisign/add";
		} else {
			model.addAttribute("apiSign", apiSign);
			return "apisign/edit";
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
		return "apisign/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param apiSign
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ApiSign apiSign) {
		Assert.notNull(apiSign, "保存数据为空");
		apiSign.setCreateUserId(getUserId());
		apiSign.setCreateTime(new Date());
		apiSign.setUpdateUserId(getUserId());
		apiSign.setUpdateTime(new Date());
		apiSign.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		apiSignService.save(apiSign);
		logger.info("【{}】保存成功", apiSign);
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
		ApiSign apiSign = apiSignService.getApiSignById(id);
		Assert.notNull(apiSign, "数据不存在");
		model.addAttribute("apiSign", apiSign);
		return "apisign/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param apiSign
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ApiSign apiSign) {
		Assert.notNull(apiSign, "修改数据为空");
		ApiSign apiSignInfo = apiSignService.getApiSignById(apiSign.getId());
		Assert.notNull(apiSignInfo, "数据不存在");
		apiSign.setUpdateUserId(getUserId());
		apiSign.setUpdateTime(new Date());
		apiSignService.update(apiSign);
		logger.info("【{}】修改成功", apiSign);
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
		ApiSign apiSign = apiSignService.getApiSignById(id);
		Assert.notNull(apiSign, "数据不存在");
		apiSignService.remove(apiSign);
		logger.info("【{}】删除成功", apiSign);
		return buildSuccess("删除成功");
	}
	
}
