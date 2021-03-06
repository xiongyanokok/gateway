package com.hexun.gateway.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;

import com.hexun.common.http.RequestPackage;
import com.hexun.common.http.ResponsePackage;
import com.hexun.common.utils.JsonUtils;
import com.hexun.gateway.disconf.CommonDisconf;
import com.hexun.gateway.pojo.ResourceInfo;
import com.hexun.hwcommon.model.CommonLoginInfo;

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.handler.codec.http.QueryStringDecoder;
import io.netty.util.CharsetUtil;

/**
 * 聚合工具类
 * 
 * @author xiongyan
 * @date 2018年1月17日 下午3:09:30
 */
public class AggregatorUtils {

	private AggregatorUtils() {
		
	}
	
	/**
	 * 提取请求参数
	 * 
	 * @param uri
	 * @return
	 */
	public static Map<String, String> getParamMap(String uri) {
		QueryStringDecoder decoder = new QueryStringDecoder(uri);
		Map<String, String> paramMap = new HashMap<>(decoder.parameters().size());
		decoder.parameters().entrySet().forEach(entry -> paramMap.put(entry.getKey(), entry.getValue().get(0)));
		return paramMap;
	}

	/**
	 * 获取聚合资源列表
	 * 
	 * @param request
	 * @param resources
	 * @return
	 */
	public static List<ResourceInfo> listResourceInfo(HttpRequest request, List<ResourceInfo> resources) {
		// 获取请求参数
		Map<String, String> paramMap = getParamMap(request.uri());

		// 聚合资源集合
		List<ResourceInfo> resourceList = new ArrayList<>();
		for (ResourceInfo resource : resources) {
			// 克隆
			ResourceInfo resourceInfo = SerializationUtils.clone(resource);
			if (resourceInfo.getIsLogin()) {
				String cookie = request.headers().get("Cookie");	
				if (StringUtils.isEmpty(cookie)) {
					continue;
				}
				String userId = getUserId(cookie);
				if (StringUtils.isEmpty(userId)) {
					continue;
				}
				resourceInfo.setCookie(cookie);
				paramMap.put("@", userId);
			}
			// 替换{xx}参数内容
			String url = replace(resourceInfo.getResourceUrl(), paramMap);
			if (StringUtils.isNotEmpty(url)) {
				resourceInfo.setResourceUrl(url);
			}
			resourceList.add(resourceInfo);
		}
		return resourceList;
	}
	
	/**
	 * 获取用户id
	 * 
	 * @param cookie
	 * @return
	 */
	public static String getUserId(String cookie) {
		Map<String, String> map = new HashMap<>(1);
		map.put("cookie", cookie);
		ResponsePackage response = RequestPackage.get(CommonDisconf.getLoginUrl()).setHeaders(map).getResponse();
		if (null == response || !response.isSuccess()) {
			return null;
		}
		
		CommonLoginInfo commonLoginInfo = JsonUtils.string2Obj(response.getContent(), CommonLoginInfo.class);
		if (null == commonLoginInfo) {
			return null;
		}
		return commonLoginInfo.getUserid();
	}

	/**
	 * url替换{xx}内容
	 *
	 * @param url
	 * @param paramMap
	 * @return
	 */
	public static String replace(String url, Map<String, String> paramMap) {
		if (MapUtils.isEmpty(paramMap)) {
			return url;
		}
		if (!url.contains("{") && !url.contains("}")) {
			return url;
		}
		// 正则替换{}内容
		String newUrl = url;
		for (Map.Entry<String, String> entry : paramMap.entrySet()) {
			newUrl = newUrl.replaceAll("\\{" + entry.getKey() + "\\}", entry.getValue());
		}
		return newUrl;
	}

	/**
	 * 输出结果
	 * 
	 * @param ctx
	 * @param result
	 */
	public static void writeAndFlush(ChannelHandlerContext ctx, String result) {
		FullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, Unpooled.copiedBuffer(result, CharsetUtil.UTF_8));
		response.headers().set(Constant.CONTENTTYPE, Constant.MEDIATYPE);
		ctx.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE);
	}

}
