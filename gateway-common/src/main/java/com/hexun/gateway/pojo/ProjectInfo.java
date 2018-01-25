package com.hexun.gateway.pojo;

import lombok.Getter;
import lombok.Setter;

/**
 * 项目信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:13
 */
@Getter
@Setter
public class ProjectInfo {

	/**
	 * 项目名称
	 */
	private String projectName;
	
	/**
	 * 统一错误信息
	 */
	private String errorMessage;

}
