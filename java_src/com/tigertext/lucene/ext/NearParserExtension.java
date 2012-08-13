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
import org.apache.lucene.spatial.tier.CartesianPolyFilterBuilder;
import org.apache.lucene.spatial.tier.DistanceFilter;
import org.apache.lucene.spatial.tier.LatLongDistanceFilter;

import com.tigertext.lucene.DocumentTranslator;
import com.tigertext.lucene.LuceneServer;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * Extension to run ".near" queries
 */
@SuppressWarnings("deprecation")
public class NearParserExtension extends ParserExtension {
	private static final Logger	jlog	= Logger.getLogger(LuceneServer.class
												.getName());

	@Override
	public Query parse(ExtensionQuery extQuery) throws ParseException {
		String key = extQuery.getField();
		String[] data = extQuery.getRawQueryString().split(",");
		if (data.length != 3) {
			throw new ParseException(
					"near queries expect values in <lat>,<long>,<miles> format");
		} else {
			try {
				double lat = Double.parseDouble(data[0]);
				double lng = Double.parseDouble(data[1]);
				double miles = Double.parseDouble(data[2]);
				String tierPrefix = key + "`tier_";

				jlog.finer("lat:" + lat + ", lng:" + lng + ", miles:" + miles
						+ ", prefix: " + tierPrefix);

				CartesianPolyFilterBuilder cpf = new CartesianPolyFilterBuilder(
						tierPrefix, DocumentTranslator.MIN_TIER,
						DocumentTranslator.MAX_TIER);

				Filter cartesianFilter = cpf.getBoundingArea(lat, lng, miles);
				DistanceFilter filter = new LatLongDistanceFilter(cartesianFilter, lat,
						lng, miles, key + "`lat", key + "`lng");

				ValueSource valSrc = new DistanceValueSource(filter);

				return new CustomScoreQuery(new ConstantScoreQuery(filter),
						new ValueSourceQuery(valSrc));
			} catch (IllegalArgumentException iae) {
				throw new ParseException(iae.getMessage());
			}
		}
	}
}
