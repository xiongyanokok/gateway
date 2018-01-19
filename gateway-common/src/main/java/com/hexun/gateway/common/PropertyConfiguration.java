package com.hexun.gateway.common;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.io.ClassPathResource;

/**
 * 加载Property
 * 
 * @author xiongyan
 * @date 2017年3月9日 上午10:22:26
 */
@Configuration
public class PropertyConfiguration {
	
	/**
	 * 加载common.properties文件内容到内存中
	 * 
	 * @return
	 */
	@Bean
    public PropertySourcesPlaceholderConfigurer createPropertySourcesPlaceholderConfigurer() {
        ClassPathResource resource = new ClassPathResource("common.properties");
        PropertySourcesPlaceholderConfigurer propertyPlaceholderConfigurer = new PropertySourcesPlaceholderConfigurer();
        propertyPlaceholderConfigurer.setLocation(resource);
        return propertyPlaceholderConfigurer;
    }
	
}
