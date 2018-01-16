package com.hexun.gateway.netty;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.HttpRequest;

/**
 * 服务端处理器
 * 
 * @author xiongyan
 * @date 2018年1月10日 上午10:20:09
 */
@Component
@Sharable
public class HttpServerInboundHandler extends ChannelInboundHandlerAdapter {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(HttpServerInboundHandler.class);
	
	@Value("${aggregator.request}")
    private String key;
	
	@Autowired
	private Map<String, AggregatorRequest> aggregatorRequestMap;

	/**
	 * 接收消息
	 * 
	 * @param ctx
	 * @param msg
	 * @throws Exception
	 */
	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		if (!(msg instanceof HttpRequest)) {
			AggregationUtils.writeAndFlush(ctx, Result.NORESOURCE.toString());
			return;
		}
		
		HttpRequest request = (HttpRequest) msg;
		String uri = request.uri();
		if (!uri.startsWith(Constant.PREFIX)) {
			AggregationUtils.writeAndFlush(ctx, Result.NORESOURCE.toString());
			return;
		}

		// 聚合名称
		String name = uri.substring(Constant.PREFIX.length());
		AggregationInfo aggregationInfo = AggregationCache.get(name);
		if (null == aggregationInfo || CollectionUtils.isEmpty(aggregationInfo.getResources())) {
			AggregationUtils.writeAndFlush(ctx, Result.NORESOURCE.toString());
			return;
		}
		
		// 获取聚合资源
		List<AggregationResource> resources = AggregationUtils.getAggregationResource(request, aggregationInfo.getResources());
		
		AggregatorRequest aggregatorRequest = aggregatorRequestMap.get(key);
		String result = null;
		if (AggregationType.PARALLEL.getValue().equals(aggregationInfo.getType())) {
			result = aggregatorRequest.parallel(resources);
		} else {
			result = aggregatorRequest.serial(resources);
		}
		
		String callback = AggregationUtils.getParamMap(uri).get(Constant.CALLBACK);
		if (StringUtils.isNotEmpty(callback)) {
			result = callback + "(" + result + ");";
		}

		AggregationUtils.writeAndFlush(ctx, result);
	}
	
	/**
	 * 异常处理
	 * 
	 * @param ctx
	 * @param cause
	 * @throws Exception
	 */
	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		logger.error("异常:", cause);
		AggregationUtils.writeAndFlush(ctx, Result.SYSTEMERROR.toString());
		ctx.close();
	}
	
}
