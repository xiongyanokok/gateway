package com.hexun.gateway;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.zuul.EnableZuulProxy;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

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

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

}
