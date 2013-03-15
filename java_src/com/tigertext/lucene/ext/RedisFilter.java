package com.tigertext.lucene.ext;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.DocIdSet;
import org.apache.lucene.search.FieldCache;
import org.apache.lucene.search.Filter;
import org.apache.lucene.util.FixedBitSet;

import com.tigertext.redis.RedisManager;

import redis.clients.jedis.Jedis;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * 
 */
public class RedisFilter extends Filter {
	private static final Logger		jlog				= Logger.getLogger(RedisFilter.class
																.getName());

	private static final long		serialVersionUID	= -7177019627513122695L;
	private String					command;
	private String					fieldName;
	private Map<Integer, Double>	scores;
	private int						nextDocBase;
	private int						port;
	private String					host;
	private int						db;

	/**
	 * @param host
	 *            Redis host
	 * @param port
	 *            Redis port
	 * @param db
	 *            Redis db
	 * @param command
	 *            Redis command to execute. It may include {%field}, which will
	 *            be replaced by the field value.
	 * @param fieldName
	 *            Lucene field to consider
	 */
	public RedisFilter(String host, int port, int db, String command,
			String fieldName) {
		this.command = command;
		this.fieldName = fieldName;
		this.host = host;
		this.port = port;
		this.db = db;
		/* store calculated scores for reuse by other components */
		this.scores = new HashMap<Integer, Double>();
	}

	@Override
	public DocIdSet getDocIdSet(IndexReader reader) throws IOException {
		final int docBase = this.nextDocBase;
		this.nextDocBase += reader.maxDoc();

		final String[] docValues = FieldCache.DEFAULT.getStrings(reader,
				this.fieldName);

		final FixedBitSet bits = new FixedBitSet(reader.maxDoc());

		String[] commandParts = this.command.split(" ");

		if (commandParts[0].toUpperCase().equals("SISMEMBER")
				&& commandParts.length == 3) {
			
			Jedis jedis = RedisManager.getInstance().getJedis(
					this.host + ":" + this.port);
			
			if (this.db != 0)
				jedis.select(this.db);

			String key = commandParts[1];
			Set<String> members = jedis.smembers(key);
			
			RedisManager.getInstance().returnJedis(this.host + ":" + this.port,
					jedis);

			for (int docid = 0; docid < docValues.length; docid++) {
				if (members == null) {
					bits.clear(docid);
				} else {
					bits.set(docid);
					scores.put(docid + docBase,
							members.contains(docValues[docid]) ? 1.0 : 0.0);
				}
			}
		} else {
			jlog.warning("Unsupported redis command: " + this.command);
		}

		jlog.finer("Bits: " + bits.getBits());
		return bits;
	}

	/**
	 * The score assigned to the doc
	 * 
	 * @param docid
	 *            Id of the doc
	 * @return null or the score given by the erlang function
	 */
	public Double getScore(int docid) {
		return this.scores.get(docid);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.command + " (doc." + this.fieldName + ")";
	}
}