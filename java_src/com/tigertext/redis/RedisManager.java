package com.tigertext.redis;

import java.util.HashMap;
import java.util.Map;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Jedis Pool manager
 */
public class RedisManager {
	private static final RedisManager	instance	= new RedisManager();
	private Map<String, JedisPool>		pools;

	private RedisManager() {
		this.pools = new HashMap<String, JedisPool>();
	}

	/**
	 * @return RedisManager singleton instance
	 */
	public final static RedisManager getInstance() {
		return instance;
	}

	private void connect(String host) {
		String[] parts = host.split(":");
		String server = parts[0];
		int port = parts.length > 1 ? Integer.valueOf(parts[1]) : 6379;

		// Create and set a JedisPoolConfig
		JedisPoolConfig poolConfig = new JedisPoolConfig();
		// Maximum active connections to Redis instance
		poolConfig.setMaxActive(10);
		// Tests whether connection is dead when connection
		// retrieval method is called
		poolConfig.setTestOnBorrow(true);
		// Tests whether connection is dead when returning a
		// connection to the pool
		poolConfig.setTestOnReturn(true);
		// Number of connections to Redis that just sit there and do nothing
		poolConfig.setMaxIdle(5);
		// Minimum number of idle connections to Redis
		// These can be seen as always open and ready to serve
		poolConfig.setMinIdle(1);
		// Tests whether connections are dead during idle periods
		poolConfig.setTestWhileIdle(true);
		// Maximum number of connections to test in each idle check
		poolConfig.setNumTestsPerEvictionRun(10);
		// Idle connection checking period
		poolConfig.setTimeBetweenEvictionRunsMillis(60000);

		// Create the jedisPool
		this.pools.put(host, new JedisPool(poolConfig, server, port));
	}

	/**
	 * Closes all connections to host
	 * 
	 * @param host
	 *            Redis host description
	 */
	public synchronized void release(String host) {
		JedisPool pool = this.pools.get(host);
		if (pool != null) {
			pool.destroy();
		}
	}

	/**
	 * Gets a connection to host
	 * 
	 * @param host
	 *            Redis host description
	 * @return a Jedis connection
	 */
	public synchronized Jedis getJedis(String host) {
		if (this.pools.get(host) == null) {
			this.connect(host);
		}
		return this.pools.get(host).getResource();
	}

	/**
	 * Returns a connection after using it
	 * 
	 * @param host
	 *            Redis host description
	 * @param jedis
	 *            connection to be returned
	 */
	public void returnJedis(String host, Jedis jedis) {
		JedisPool pool = this.pools.get(host);
		if (pool != null) {
			pool.returnResource(jedis);
		}
	}
}