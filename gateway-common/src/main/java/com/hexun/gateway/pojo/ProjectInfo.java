package com.hexun.gateway.pojo;

/**
 * 项目信息
 * 
 * @author xiongyan
 * @date 2017年12月26日 下午3:39:13
 */
public class ProjectInfo {

	/**
	 * 项目名称
	 */
	private String projectName;
	
	/**
	 * 统一错误信息
	 */
	private String errorMessage;

	public String getProjectName() {
		return projectName;
	}

	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
	
}
