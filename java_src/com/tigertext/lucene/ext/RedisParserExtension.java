package com.tigertext.lucene.ext;

import java.util.logging.Logger;

import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.ext.ExtensionQuery;
import org.apache.lucene.queryParser.ext.ParserExtension;
import org.apache.lucene.search.ConstantScoreQuery;
import org.apache.lucene.search.Filter;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.function.CustomScoreQuery;
import org.apache.lucene.search.function.ValueSource;
import org.apache.lucene.search.function.ValueSourceQuery;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Extension to run
 *         ".redis" queries. These queries should be in the form
 *         "Field.redis:[REDIS-COMMAND]", where [REDIS-COMMAND] can include a
 *         {%field} value that will be replaced by the field value. The redis
 *         command will then be executed and its result is expected to be a
 *         number which will be considered the resulting score
 */
public class RedisParserExtension extends ParserExtension {
	private static final Logger	jlog	= Logger.getLogger(ErlangParserExtension.class
												.getName());

	@Override
	public Query parse(ExtensionQuery extQuery) throws ParseException {
		String key = extQuery.getField();
		String[] parts = extQuery.getRawQueryString().split("[|]");
		String command = parts[0];
		String host = "localhost";
		int port = 6379;
		int db = 0;

		if (parts.length > 1) {
			String[] serverParts = parts[1].split(":");
			host = serverParts[0];
			if (serverParts.length > 1)
				port = Integer.valueOf(serverParts[1]);
			if (serverParts.length > 2)
				db = Integer.valueOf(serverParts[2]);
		}

		jlog.finer("redis query against " + host + ":" + port + " [" + db
				+ "] using " + command + "...");

		Filter filter = new RedisFilter(host, port, db, command, key);

		ValueSource valSrc = new RedisValueSource((RedisFilter) filter);

		return new CustomScoreQuery(new ConstantScoreQuery(filter),
				new ValueSourceQuery(valSrc));
	}
}