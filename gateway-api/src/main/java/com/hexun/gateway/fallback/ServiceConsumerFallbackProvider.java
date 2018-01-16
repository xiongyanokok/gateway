package com.hexun.gateway.fallback;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.netflix.zuul.filters.route.FallbackProvider;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpResponse;

import com.hexun.gateway.pojo.Result;

/**
 * 服务回退
 * 
 * @author xiongyan
 * @date 2017年12月19日 上午9:52:29
 */
@Configuration
public class ServiceConsumerFallbackProvider implements FallbackProvider {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ServiceConsumerFallbackProvider.class);

	@Override
	public String getRoute() {
		return "*";
	}

	@Override
	public ClientHttpResponse fallbackResponse() {
		logger.error("xxxxxxx");
		return new ClientHttpResponse() {
			
            @Override
            public HttpStatus getStatusCode() throws IOException {
                return HttpStatus.OK;
            }
 
            @Override
            public int getRawStatusCode() throws IOException {
                return this.getStatusCode().value();
            }
 
            @Override
            public String getStatusText() throws IOException {
                return this.getStatusCode().getReasonPhrase();
            }
 
            @Override
            public void close() {
 
            }
 
            @Override
            public InputStream getBody() throws IOException {
                return new ByteArrayInputStream(Result.SERVERERROR.toString().getBytes());
            }
 
            @Override
            public HttpHeaders getHeaders() {
                HttpHeaders headers = new HttpHeaders();
                headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
                return headers;
            }
        };
	}

	@Override
	public ClientHttpResponse fallbackResponse(Throwable cause) {
		logger.error("yyyyyy：", cause);
		return new ClientHttpResponse() {
			
            @Override
            public HttpStatus getStatusCode() throws IOException {
                return HttpStatus.OK;
            }
 
            @Override
            public int getRawStatusCode() throws IOException {
                return this.getStatusCode().value();
            }
 
            @Override
            public String getStatusText() throws IOException {
                return this.getStatusCode().getReasonPhrase();
            }
 
            @Override
            public void close() {
 
            }
 
            @Override
            public InputStream getBody() throws IOException {
                return new ByteArrayInputStream(Result.SERVERERROR.toString().getBytes());
            }
 
            @Override
            public HttpHeaders getHeaders() {
                HttpHeaders headers = new HttpHeaders();
                headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
                return headers;
            }
        };
	}

	
}