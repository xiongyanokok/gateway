package com.hexun.gateway.netty;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.dianping.cat.Cat;
import com.dianping.cat.CatConstants;
import com.dianping.cat.message.Message;
import com.dianping.cat.message.Transaction;
import com.google.common.util.concurrent.RateLimiter;
import com.hexun.gateway.aggregator.AggregatorRequest;
import com.hexun.gateway.common.AggregatorCache;
import com.hexun.gateway.common.AggregatorUtils;
import com.hexun.gateway.common.CatUtils;
import com.hexun.gateway.common.Constant;
import com.hexun.gateway.disconf.CommonDisconf;
import com.hexun.gateway.enums.AggregatorTypeEnum;
import com.hexun.gateway.exception.GatewayException;
import com.hexun.gateway.pojo.AggregatorInfo;
import com.hexun.gateway.pojo.ResourceInfo;
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
	 * 聚合请求
	 */
	@Autowired
	private Map<String, AggregatorRequest<?>> aggregatorRequestMap;
	
	/**
	 * 每秒钟发放1000个令牌
	 */
	private RateLimiter limiter = RateLimiter.create(1000);

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
		if (Constant.FAVICON.equals(uri)) {
			return;
		}
		
		// 客户端
		String requestClient = CommonDisconf.getRequestClient();
		// 非netty客户端需要消费令牌
		if (!requestClient.startsWith(Constant.NETTY) && !limiter.tryAcquire(1, TimeUnit.SECONDS)) {
			// 等待1秒钟也无法取得令牌，直接返回失败
			result = Result.BUSYERROR.toString();
			return;
		}
		
		Transaction t = Cat.newTransaction(CatConstants.TYPE_URL, uri);
		CatUtils.catLog(ctx, request);
		try {
			// apigw前缀
			if (!uri.startsWith(Constant.PREFIX)) {
				return;
			}

			// 聚合名称
			String name = uri.substring(Constant.PREFIX.length());
			int index = name.indexOf('?');
			if (index != -1) {
				name = name.substring(0, index);
			}
			AggregatorInfo aggregatorInfo = AggregatorCache.get(name);
			if (null == aggregatorInfo || CollectionUtils.isEmpty(aggregatorInfo.getResourceInfos())) {
				return;
			}

			// 获取聚合资源
			List<ResourceInfo> resources = AggregatorUtils.listResourceInfo(request, aggregatorInfo.getResourceInfos());

			// 获取客户端
			AggregatorRequest<?> aggregatorRequest = aggregatorRequestMap.get(requestClient);
			if (null == aggregatorRequest) {
				throw new GatewayException("聚合请求客户端【"+ CommonDisconf.getRequestClient() + "】不存在");
			}
			
			// 执行
			if (AggregatorTypeEnum.PARALLEL.getValue().equals(aggregatorInfo.getType())) {
				result = aggregatorRequest.parallel(resources);
			} else {
				result = aggregatorRequest.serial(resources);
			}
			// 成功
			t.setStatus(Message.SUCCESS);
		} catch (Exception e) {
			// 失败
			t.setStatus(e);
			result = Result.SYSTEMERROR.toString();
			logger.error("【{}】聚合失败:", uri, e);
		} finally {
			// 完成
			t.complete();
			if (uri.contains(Constant.CALLBACK)) {
				String callback = AggregatorUtils.getParamMap(uri).get(Constant.CALLBACK);
				result = callback + "(" + result + ");";
			}
			AggregatorUtils.writeAndFlush(ctx, result);
		}
	}
	
}
