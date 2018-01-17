package com.hexun.gateway.netty;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.hexun.gateway.aggregator.AggregatorRequest;
import com.hexun.gateway.common.AggregationCache;
import com.hexun.gateway.common.AggregationUtils;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.pojo.AggregationInfo;
import com.hexun.gateway.pojo.AggregationInfo.AggregationType;
import com.hexun.gateway.pojo.AggregationResource;
import com.hexun.gateway.pojo.Result;

import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.handler.codec.http.HttpRequest;

/**
 * 服务端处理器
 * 
 * @author xiongyan
 * @date 2018年1月10日 上午10:20:09
 */
@Component
@Sharable
public class HttpServerInboundHandler extends SimpleChannelInboundHandler<HttpRequest> {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(HttpServerInboundHandler.class);

	/**
	 * 客户端
	 */
	@Value("${aggregator.request}")
	private String key;

	/**
	 * 聚合请求
	 */
	@Autowired
	private Map<String, AggregatorRequest<?>> aggregatorRequestMap;

	/**
	 * 接收消息
	 * 
	 * @param ctx
	 * @param request
	 * @throws Exception
	 */
	@Override
	public void channelRead0(ChannelHandlerContext ctx, HttpRequest request) throws Exception {
		String result = Result.NORESOURCE.toString();
		String uri = request.uri();
		try {
			// apigw前缀
			if (!uri.startsWith(Constant.PREFIX)) {
				return;
			}

			// 聚合名称
			String name = uri.substring(Constant.PREFIX.length());
			if (name.contains("?")) {
				name = name.substring(0, name.indexOf('?'));
			}
			AggregationInfo aggregationInfo = AggregationCache.get(name);
			if (null == aggregationInfo || CollectionUtils.isEmpty(aggregationInfo.getResources())) {
				return;
			}

			// 获取聚合资源
			List<AggregationResource> resources = AggregationUtils.getAggregationResource(request, aggregationInfo.getResources());

			// 获取客户端
			AggregatorRequest<?> aggregatorRequest = aggregatorRequestMap.get(key);
			if (AggregationType.PARALLEL.getValue().equals(aggregationInfo.getType())) {
				result = aggregatorRequest.parallel(resources);
			} else {
				result = aggregatorRequest.serial(resources);
			}
		} catch (Exception e) {
			result = Result.SYSTEMERROR.toString();
			logger.error("【{}】聚合失败:", uri, e);
		} finally {
			if (uri.contains(Constant.CALLBACK)) {
				String callback = AggregationUtils.getParamMap(uri).get(Constant.CALLBACK);
				result = callback + "(" + result + ");";
			}
			AggregationUtils.writeAndFlush(ctx, result);
		}
	}

}
