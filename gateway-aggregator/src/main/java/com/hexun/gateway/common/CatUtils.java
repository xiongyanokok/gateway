package com.hexun.gateway.common;

import java.net.InetSocketAddress;

import com.dianping.cat.Cat;
import com.dianping.cat.CatConstants;
import com.dianping.cat.message.Message;

import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpRequest;

/**
 * cat 日志
 * 
 * @author xiongyan
 * @date 2018年1月22日 下午2:29:24
 */
public class CatUtils {
	
	private CatUtils() {
		
	}
	
	public static void catLog(ChannelHandlerContext ctx, HttpRequest request) {
		try {
			logRequestClientInfo(ctx, request);
			logRequestPayload(request);
		} catch (Exception e) {
			// 
		}
	}

	private static void logRequestClientInfo(ChannelHandlerContext ctx, HttpRequest request) {
		StringBuilder sb = new StringBuilder(1024);
		String ip = request.headers().get("X-Forwarded-For");
		if (ip == null) {
			ip = getHostAddress(ctx);
		}
		sb.append("IPS=").append(ip);
		sb.append("&VirtualIP=").append(getHostAddress(ctx));
		sb.append("&Referer=").append(request.headers().get("Host"));
		sb.append("&Agent=").append(request.headers().get("User-Agent"));
		Cat.logEvent(CatConstants.TYPE_URL, CatConstants.TYPE_URL + ".Server", Message.SUCCESS, sb.toString());
	}

	private static void logRequestPayload(HttpRequest request) {
		StringBuilder sb = new StringBuilder(256);
		sb.append(request.protocolVersion().text()).append('/');
		sb.append(request.method()).append(' ').append(request.uri());
		Cat.logEvent(CatConstants.TYPE_URL, CatConstants.TYPE_URL + ".Method", Message.SUCCESS, sb.toString());
	}
	
	private static String getHostAddress(ChannelHandlerContext ctx) {
		InetSocketAddress insocket = (InetSocketAddress) ctx.channel().remoteAddress();
		return insocket.getAddress().getHostAddress();
	}
}
