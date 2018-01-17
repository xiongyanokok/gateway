package com.hexun.gateway.common;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * 线程池
 * 
 * @author xiongyan
 * @date 2017年8月29日 上午11:39:06
 */
public final class ThreadPoolContext {
	
	private ThreadPoolContext() {
		
	}
	
	/**
	 * 线程池
	 */
	private static final ExecutorService threadPool = new ThreadPoolExecutor(10, 100, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
	
	/**
	 * 异步执行
	 * 
	 * @param callable
	 * @return
	 */
	public static <T> Future<T> submit(Callable<T> callable) {
		return threadPool.submit(callable);
	}
	
	/**
	 * 异步执行
	 * 
	 * @param runnable
	 */
	public static void execute(Runnable runnable) {
		threadPool.execute(runnable);
	}
	
	/**
	 * 关闭线程池
	 */
	public static void shutdown() {
		threadPool.shutdown();
	}
	
}
