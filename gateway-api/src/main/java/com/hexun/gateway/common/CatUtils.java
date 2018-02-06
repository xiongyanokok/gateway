package com.hexun.gateway.common;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;

import com.dianping.cat.Cat;
import com.dianping.cat.CatConstants;
import com.dianping.cat.message.Message;

/**
 * cat 日志
 * 
 * @author xiongyan
 * @date 2018年1月22日 下午2:29:24
 */
public class CatUtils {
	
	private CatUtils() {
		
	}
	
	public static void catLog(HttpServletRequest request) {
		try {
			logRequestClientInfo(request);
			logRequestPayload(request);
		} catch (Exception e) {
			// 
		}
	}

	private static void logRequestClientInfo(HttpServletRequest request) {
        StringBuilder sb = new StringBuilder(1024);
        String ip = request.getHeader("x-forwarded-for");
        if (StringUtils.isNotEmpty(ip)) {
            ip = request.getRemoteAddr();
        }
        sb.append("IPS=").append(ip);
        sb.append("&VirtualIP=").append(request.getRemoteAddr());
        sb.append("&Server=").append(request.getServerName());
        sb.append("&Referer=").append(request.getHeader("referer"));
        sb.append("&Agent=").append(request.getHeader("user-agent"));
        Cat.logEvent(CatConstants.TYPE_URL, CatConstants.TYPE_URL + ".Server", Message.SUCCESS, sb.toString());
    }

	private static void logRequestPayload(HttpServletRequest request) {
        StringBuilder sb = new StringBuilder(256);
        sb.append(request.getScheme().toUpperCase()).append('/');
        sb.append(request.getMethod()).append(' ').append(request.getRequestURI());
        String qs = request.getQueryString();
        if (StringUtils.isNotEmpty(qs)) {
            sb.append('?').append(qs);
        }
        Cat.logEvent(CatConstants.TYPE_URL, CatConstants.TYPE_URL + ".Method", Message.SUCCESS, sb.toString());
    }
	
}
