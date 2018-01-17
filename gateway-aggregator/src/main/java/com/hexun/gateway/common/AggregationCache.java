package com.hexun.gateway.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.hexun.gateway.pojo.AggregationInfo;
import com.hexun.gateway.pojo.AggregationResource;

/**
 * 聚合配置信息
 * 
 * @author xiongyan
 * @date 2018年1月17日 下午3:09:12
 */
public final class AggregationCache {

	private AggregationCache() {
		
	}

	private static final Map<String, AggregationInfo> MAP = new HashMap<>();
	
	public static void put(AggregationInfo aggregationinfo1) {
		MAP.put(aggregationinfo1.getName(), aggregationinfo1);
	}
	
	public static AggregationInfo get(String key) {
		return MAP.get(key);
	}
	
	public static List<AggregationInfo> init() {
		AggregationInfo info1 = new AggregationInfo();
		info1.setName("user-center");
		info1.setUri("user-center");
		info1.setType(1);
		
		List<AggregationResource> resources1 = new ArrayList<>();
		info1.setResources(resources1);
		
		AggregationResource resource1 = new AggregationResource();
		resource1.setName("user-center");
		resource1.setResourceName("action");
		resource1.setResourceUrl("http://caidaoaction.intcoop.hexun.com/msg/getNoReadActionNum.do");
		resource1.setResourceMethod("GET");
		resource1.setIsLogin(true);
		resource1.setTimeOut(5);
		resource1.setIsCache(false);
		resource1.setResourceTemplate("<%var statecode = @action.get(\"statecode\").asText(); if(statecode == \"1\") {%> ${@action.get(\"result\")} <%} else {%> 0 <%}%>");
		resources1.add(resource1);
		
		AggregationResource resource2 = new AggregationResource();
		resource2.setName("user-center");
		resource2.setResourceName("coins");
		resource2.setResourceUrl("http://testvm.tool.hexun.com/rest/vmcenter/user-info2");
		resource2.setResourceMethod("GET");
		resource2.setIsLogin(true);
		resource2.setTimeOut(5);
		resource2.setIsCache(false);
		resource2.setResourceTemplate("<%var code = @coins.get(\"code\").intValue(); if(code == 200) {%> ${@coins.get(\"result\").get(\"account\")} <%} else {%> 0 <%}%>");
		resources1.add(resource2);
		
		AggregationResource resource3 = new AggregationResource();
		resource3.setName("user-center");
		resource3.setResourceName("favorites");
		resource3.setResourceUrl("http://test.apicaidao.hexun.com/favorites/total");
		resource3.setResourceMethod("GET");
		resource3.setIsLogin(true);
		resource3.setTimeOut(5);
		resource3.setIsCache(false);
		resource3.setResourceTemplate("<%var code = @favorites.get(\"code\").intValue(); if(code == 0) {%> ${@favorites.get(\"data\")} <%} else {%> 0 <%}%>");
		resources1.add(resource3);
		
		AggregationResource resource4 = new AggregationResource();
		resource4.setName("user-center");
		resource4.setResourceName("purse");
		resource4.setResourceUrl("http://test.apicaidao.hexun.com/api/balance");
		resource4.setResourceMethod("GET");
		resource4.setIsLogin(true);
		resource4.setTimeOut(5);
		resource4.setIsCache(false);
		resource4.setResourceTemplate("<%var code = @purse.get(\"code\").intValue(); var success = @purse.get(\"data\").get(\"is_success\").asText(); if(code == 0 && success == 'T') {%> ${@purse.get(\"data\").get(\"account_list\")} <%} else {%> null <%}%>");
		resources1.add(resource4);
		
		AggregationResource resource5 = new AggregationResource();
		resource5.setName("user-center");
		resource5.setResourceName("relation");
		resource5.setResourceUrl("http://testfollow.zq.hexun.com/relation/getrelationinfo1.do?uid={@}&source=2");
		resource5.setResourceMethod("GET");
		resource5.setIsLogin(true);
		resource5.setTimeOut(5);
		resource5.setIsCache(false);
		resource5.setResourceTemplate("<%var statecode = @relation.get(\"statecode\").asText(); if(statecode == \"1\") {%> ${@relation.get(\"result\").get(\"attentionCount\")} <%} else {%> 0 <%}%>");
		resources1.add(resource5);
		
		
		AggregationInfo info2 = new AggregationInfo();
		info2.setName("live-room");
		info2.setUri("live-room");
		info2.setType(2);
		
		List<AggregationResource> resources2 = new ArrayList<>();
		info2.setResources(resources2);
		
		AggregationResource resource12 = new AggregationResource();
		resource12.setResourceIndex(1);
		resource12.setName("live-room");
		resource12.setResourceName("myclassteacher");
		resource12.setResourceUrl("http://test.apilesson.hexun.com/user/myclassteacher");
		resource12.setResourceMethod("GET");
		resource12.setIsLogin(true);
		resource12.setTimeOut(5);
		resource12.setIsCache(false);
		resources2.add(resource12);
		
		AggregationResource resource22 = new AggregationResource();
		resource22.setResourceIndex(2);
		resource22.setName("live-room");
		resource22.setResourceName("myviprooms");
		resource22.setResourceUrl("http://test.api.zhibo.hexun.com/api/caidao/user/get_my_vip_rooms_and_teachers_rooms?teacherIds={teacherIds}");
		resource22.setResourceMethod("GET");
		resource22.setIsLogin(true);
		resource22.setTimeOut(5);
		resource22.setIsCache(false);
		resource22.setResourceRegex("<%var code = @myviprooms.get(\"code\").intValue(); if(code == 0) {%> {\"teacherIds\":${@myviprooms.get(\"data\")}} <%} else {%> {\"teacherIds\":11} <%}%>");
		resources2.add(resource22);
		
		return Arrays.asList(info1, info2);
	}
}
