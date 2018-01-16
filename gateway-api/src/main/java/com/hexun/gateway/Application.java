package com.hexun.gateway;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.zuul.EnableZuulProxy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

import com.hexun.gateway.filter.AccessFilter;
import com.hexun.gateway.filter.CacheFilter;
import com.hexun.gateway.filter.LockFilter;
import com.hexun.gateway.filter.LoginFilter;
import com.hexun.gateway.filter.SignFilter;
import com.hexun.gateway.filter.SuccessFilter;
import com.hexun.gateway.filter.UnLockFilter;
import com.hexun.gateway.filter.XssFilter;

/**
 * 启动服务
 * 
 * @author xiongyan
 * @date 2017年12月17日 上午11:01:57
 */
@SpringBootApplication
@EnableZuulProxy
@ComponentScan("com.hexun")
@ImportResource("classpath:/spring/applicationContext-*.xml")
public class Application {

	/**
	 * -2
	 * 
	 * @return
	 */
	@Bean
	public AccessFilter accessFilter() {
		return new AccessFilter();
	}
	
	/**
	 * 1
	 * 
	 * @return
	 */
	@Bean
	public LoginFilter loginFilter() {
		return new LoginFilter();
	}
	
	/**
	 * 2
	 * 
	 * @return
	 */
	@Bean
	public XssFilter xssFilter() {
		return new XssFilter();
	}
	
	/**
	 * 3
	 * 
	 * @return
	 */
	@Bean
	public SignFilter signFilter() {
		return new SignFilter();
	}
	
	/**
	 * 4
	 * 
	 * @return
	 */
	@Bean
	public LockFilter lockFilter() {
		return new LockFilter();
	}
	
	/**
	 * 5
	 * 
	 * @return
	 */
	@Bean
	public CacheFilter cacheFilter() {
		return new CacheFilter();
	}
	
	/**
	 * 998
	 * 
	 * @return
	 */
	@Bean
	public UnLockFilter unLockFilter() {
		return new UnLockFilter();
	}
	
	/**
	 * 999
	 * 
	 * @return
	 */
	@Bean
	public SuccessFilter successFilter() {
		return new SuccessFilter();
	}
	
	
	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

}
