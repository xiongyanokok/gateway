package com.hexun.gateway.config;

import java.util.Arrays;

import javax.sql.DataSource;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.io.ClassPathResource;

import com.alibaba.druid.pool.DruidDataSource;
import com.alibaba.druid.wall.WallConfig;
import com.alibaba.druid.wall.WallFilter;

/**
 * druid数据源
 * 
 * @author xiongyan
 * @date 2017年3月9日 上午10:22:26
 */
@Configuration
public class DataSourceConfiguration {
	
	/**
	 * 加载jdbc.properties文件内容到内存中
	 * 
	 * @return
	 */
	@Bean
    public PropertySourcesPlaceholderConfigurer createPropertySourcesPlaceholderConfigurer() {
        ClassPathResource jdbcResource = new ClassPathResource("jdbc.properties");
        ClassPathResource commonResource = new ClassPathResource("common.properties");
        PropertySourcesPlaceholderConfigurer propertyPlaceholderConfigurer = new PropertySourcesPlaceholderConfigurer();
        propertyPlaceholderConfigurer.setLocations(jdbcResource, commonResource);
        return propertyPlaceholderConfigurer;
    }
	
	/**
	 * 数据源
	 * 
	 * @return
	 */
	@Bean(name = "dataSource", destroyMethod = "close", initMethod = "init")
	@ConfigurationProperties(prefix = "spring.datasource")
	public DataSource dataSource() {
		DruidDataSource dataSource = new DruidDataSource();
		WallConfig wallConfig = new WallConfig();
		wallConfig.setMultiStatementAllow(true);
		WallFilter wallFilter = new WallFilter();
		wallFilter.setConfig(wallConfig);
		dataSource.setProxyFilters(Arrays.asList(wallFilter));
		return dataSource;
	}

}
