package com.hexun.gateway.common;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.redisson.api.RBucket;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hexun.cache.IRedisClient;

/**
 * 线程池
 * 
 * @author xiongyan
 * @date 2017年8月29日 上午11:39:06
 */
public final class ThreadPoolContext {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ThreadPoolContext.class);
	
	private ThreadPoolContext() {
		
	}
	
	/**
	 * 线程池
	 */
	private static final ExecutorService THREADPOOL = new ThreadPoolExecutor(10, 100, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<Runnable>(1000));
	
	/**
	 * redisClient
	 */
	private static IRedisClient redisClient; 
	
	/**
	 * 初始化redisClient
	 * 
	 * @param redisClient
	 */
	public static void setRedisClient(IRedisClient redisClient) {
		ThreadPoolContext.redisClient = redisClient;
	}
	
	/**
	 * 关闭线程池
	 * 
	 */
	public static void shutdown() {
		ThreadPoolContext.THREADPOOL.shutdown();
	}
	
	/**
	 * 异步执行
	 * 
	 * @param key
	 * @param duration
	 * @param callable
	 * @return
	 */
	public static <T> Future<T> submit(final String key, final int duration, final Callable<T> callable) {
		return THREADPOOL.submit(new Callable<T>() {

			@Override
			public T call() throws Exception {
				return cacheCall(key, duration, callable);
			}
		});
	}
	
	/**
	 * 异步执行
	 * 
	 * @param callable
	 * @return
	 */
	public static <T> Future<T> submit(Callable<T> callable) {
		return THREADPOOL.submit(callable);
	}

	/**
	 * 异步执行
	 * 
	 * @param runnable
	 */
	public static void execute(Runnable runnable) {
		THREADPOOL.execute(runnable);
	}
	
	/**
	 * 获取异步执行结果
	 * 
	 * @param future
	 * @return
	 */
	public static <T> T getResult(Future<T> future) {
		try {
			return future.get();
		} catch (Exception e) {
			logger.error("异步查询失败", e);
			return null;
		}
	}
	
	/**
	 * 获取异步执行结果
	 * 
	 * @param future
	 * @param timeout
	 * @return
	 */
	public static <T> T getResult(Future<T> future, int timeout) {
		try {
			return future.get(timeout, TimeUnit.MILLISECONDS);
		} catch (Exception e) {
			logger.error("异步查询失败", e);
			return null;
		}
	}
	
	/**
	 * 获取缓存结果
	 * 
	 * @param key
	 * @param duration
	 * @param callable
	 * @return
	 * @throws Exception
	 */
	private static <T> T cacheCall(String key, int duration, Callable<T> callable) throws Exception {
		RBucket<T> rBucket = redisClient.getBucket(key);
		// 获取缓存
		T t = rBucket.get();
		if (null != t) {
			return t;
		}
		// 获取数据
		t = callable.call();
		if (null != t) {
			// 设置缓存
			rBucket.set(t, duration, TimeUnit.MINUTES);
		}
		return t;
	}
	
}
