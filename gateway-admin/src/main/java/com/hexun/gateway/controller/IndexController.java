package com.hexun.gateway.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.hexun.gateway.common.utils.CommonUtils;
import com.hexun.gateway.enums.MethodEnum;
import com.hexun.gateway.enums.TimeUnitEnum;
import com.hexun.gateway.model.Aggregator;
import com.hexun.gateway.model.AggregatorResource;
import com.hexun.gateway.model.ApiCache;
import com.hexun.gateway.model.ApiInfo;
import com.hexun.gateway.model.ApiIp;
import com.hexun.gateway.model.ApiProject;
import com.hexun.gateway.model.ApiRateLimit;
import com.hexun.gateway.model.ApiSign;
import com.hexun.gateway.model.ApiUser;
import com.hexun.gateway.pojo.AggregatorInfo;
import com.hexun.gateway.pojo.CacheInfo;
import com.hexun.gateway.pojo.GatewayInfo;
import com.hexun.gateway.pojo.ProjectInfo;
import com.hexun.gateway.pojo.RateLimitInfo;
import com.hexun.gateway.pojo.ResourceInfo;
import com.hexun.gateway.pojo.SignInfo;
import com.hexun.gateway.service.AggregatorResourceService;
import com.hexun.gateway.service.AggregatorService;
import com.hexun.gateway.service.ApiCacheService;
import com.hexun.gateway.service.ApiInfoService;
import com.hexun.gateway.service.ApiIpService;
import com.hexun.gateway.service.ApiProjectService;
import com.hexun.gateway.service.ApiRateLimitService;
import com.hexun.gateway.service.ApiSignService;
import com.hexun.gateway.service.ApiUserService;

/**
 * Controller
 * 
 * @author admin
 * @date 2016年11月17日 下午2:06:29
 */
@Controller
@RequestMapping(value = "/", produces = {"application/json; charset=UTF-8"})
public class IndexController extends BaseController {
	
	@Autowired
	private ApiInfoService apiInfoService;
	
	@Autowired
	private ApiProjectService apiProjectService;
	
	@Autowired
	private ApiCacheService apiCacheService;
	
	@Autowired
	private ApiSignService apiSignService;
	
	@Autowired
	private ApiRateLimitService apiRateLimitService;
	
	@Autowired
	private ApiIpService apiIpService;
	
	@Autowired
	private ApiUserService apiUserService;
	
	@Autowired
	private AggregatorService aggregatorService;
	
	@Autowired
	private AggregatorResourceService aggregatorResourceService;
	
    /**
	 * 首页
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/", method = {RequestMethod.GET})
	public String index(Model model) {
		return "index";
	}
	
	/**
	 * 网关信息
	 * 
	 * @return
	 */
	@RequestMapping(value = "/gateway", method = { RequestMethod.GET })
	@ResponseBody
	public List<GatewayInfo> gatewayInfo() {
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		List<ApiInfo> apiInfos = apiInfoService.listApiInfo(map);
		if (CollectionUtils.isEmpty(apiInfos)) {
			return Collections.emptyList();
		}
		
		List<ApiProject> apiProjects = apiProjectService.listApiProject(map);
		
		List<ApiCache> apiCaches = apiCacheService.listApiCache(map);
		
		List<ApiSign> apiSigns = apiSignService.listApiSign(map);
		
		List<ApiRateLimit> apiRateLimits = apiRateLimitService.listApiRateLimit(map);
		
		List<ApiIp> apiIps = apiIpService.listApiIp(map);
		
		List<ApiUser> apiUsers = apiUserService.listApiUser(map);
		
		List<GatewayInfo> gatewayInfos = new ArrayList<>(apiInfos.size());
		for (ApiInfo apiInfo : apiInfos) {
			GatewayInfo gatewayInfo = new GatewayInfo();
			gatewayInfo.setUri(apiInfo.getUri());
			gatewayInfo.setEnabled(apiInfo.getEnabled());
			gatewayInfo.setMethod(MethodEnum.getMethod(apiInfo.getMethod()));
			gatewayInfo.setLogin(apiInfo.getLogin());
			gatewayInfo.setSign(apiInfo.getSign());
			gatewayInfo.setCache(apiInfo.getGlobalCache());
			gatewayInfo.setLock(apiInfo.getGlobalLock());
			gatewayInfo.setRateLimit(apiInfo.getRateLimit());
			gatewayInfo.setMonitor(apiInfo.getMonitor());
			gatewayInfo.setLog(apiInfo.getLog());
			gatewayInfos.add(gatewayInfo);
			
			for (ApiProject apiProject : apiProjects) {
				if (apiInfo.getProjectId().equals(apiProject.getId())) {
					ProjectInfo projectInfo = new ProjectInfo();
					projectInfo.setProjectName(apiProject.getProjectEn());
					projectInfo.setErrorMessage(apiProject.getErrorMessage());
					gatewayInfo.setProjectInfo(projectInfo);
				}
			}
			
			if (CollectionUtils.isNotEmpty(apiCaches)) {
				for (ApiCache apiCache : apiCaches) {
					if (apiInfo.getId().equals(apiCache.getApiId())) {
						CacheInfo cacheInfo = new CacheInfo();
						cacheInfo.setTime(apiCache.getTime());
						cacheInfo.setTimeUnit(TimeUnitEnum.getTimeUnit(apiCache.getTimeUnit()));
						gatewayInfo.setCacheInfo(cacheInfo);
					}
				}
			}
			
			if (CollectionUtils.isNotEmpty(apiSigns)) {
				for (ApiSign apiSign : apiSigns) {
					if (apiInfo.getId().equals(apiSign.getApiId())) {
						SignInfo signInfo = new SignInfo();
						signInfo.setSignField(apiSign.getSignField());
						signInfo.setSignMethod(apiSign.getSignMethod());
						signInfo.setSignKey(apiSign.getSignKey());
						gatewayInfo.setSignInfo(signInfo);
					}
				}
			}
			
			if (CollectionUtils.isNotEmpty(apiRateLimits)) {
				for (ApiRateLimit apiRateLimit : apiRateLimits) {
					if (apiInfo.getId().equals(apiRateLimit.getApiId())) {
						RateLimitInfo rateLimitInfo = new RateLimitInfo();
						rateLimitInfo.setLimit(apiRateLimit.getNumLimit());
						rateLimitInfo.setQuota(apiRateLimit.getTimeQuota());
						rateLimitInfo.setRefreshInterval(apiRateLimit.getRefreshInterval());
						gatewayInfo.setRateLimitInfo(rateLimitInfo);
					}
				}
			}
			
			if (CollectionUtils.isNotEmpty(apiIps)) {
				Set<String> ips = new HashSet<>(); 
				for (ApiIp apiIp : apiIps) {
					if (apiInfo.getId().equals(apiIp.getApiId()) || apiIp.getApiId() == 0) {
						ips.add(apiIp.getIp());
					}
				}
				gatewayInfo.setIps(ips);
			}
			
			if (CollectionUtils.isNotEmpty(apiUsers)) {
				Set<Long> userIds = new HashSet<>();
				for (ApiUser apiUser : apiUsers) {
					if (apiInfo.getId().equals(apiUser.getApiId()) || apiUser.getApiId() == 0) {
						userIds.add(apiUser.getUserId());
					}
				}
				gatewayInfo.setUserIds(userIds);
			}
		}
		
		return gatewayInfos;
	}
	
	/**
	 * 聚合信息
	 * 
	 * @return
	 */
	@RequestMapping(value = "/aggregator", method = { RequestMethod.GET })
	@ResponseBody
	public List<AggregatorInfo> aggregatorInfo() {
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		List<Aggregator> aggregators = aggregatorService.listAggregator(map);
		if (CollectionUtils.isEmpty(aggregators)) {
			return Collections.emptyList();
		}
		
		List<AggregatorResource> aggregatorResources = aggregatorResourceService.listAggregatorResource(map);
		
		List<AggregatorInfo> aggregatorInfos = new ArrayList<>();
		for (Aggregator aggregator : aggregators) {
			AggregatorInfo aggregatorInfo = new AggregatorInfo();
			aggregatorInfo.setName(aggregator.getName());
			aggregatorInfo.setUri(aggregator.getUri());
			aggregatorInfo.setType(aggregator.getType());
			aggregatorInfos.add(aggregatorInfo);
			
			List<ResourceInfo> resourceInfos = new ArrayList<>();
			aggregatorInfo.setResourceInfos(resourceInfos);
			for (AggregatorResource resource : aggregatorResources) {
				if (aggregator.getId() == resource.getAggregatorId()) {
					ResourceInfo resourceInfo = new ResourceInfo();
					resourceInfo.setName(aggregator.getName());
					resourceInfo.setResourceIndex(resource.getResourceIndex());
					resourceInfo.setResourceName(resource.getResourceName());
					resourceInfo.setOriginalUrl(resource.getResourceUrl());
					resourceInfo.setResourceUrl(resource.getResourceUrl());
					resourceInfo.setResourceMethod(resource.getResourceMethod());
					resourceInfo.setIsLogin(resource.getIsLogin());
					resourceInfo.setTimeOut(resource.getTimeOut());
					resourceInfo.setIsCache(resource.getIsCache());
					resourceInfo.setCacheTime(resource.getCacheTime());
					resourceInfo.setResultTemplate(resource.getResultTemplate());
					resourceInfo.setParamTemplate(resource.getParamTemplate());
					resourceInfo.setDefaultValue(resource.getDefaultValue());
					resourceInfos.add(resourceInfo);
				}
			}
		}
		return aggregatorInfos;
	}
    
}
