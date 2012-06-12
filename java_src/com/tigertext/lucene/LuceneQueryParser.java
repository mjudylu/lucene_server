package com.tigertext.lucene;

import java.util.Map;
import java.util.logging.Logger;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.NumericRangeQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.util.Version;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;

public class LuceneQueryParser extends QueryParser {
	private static final Logger	jlog	= Logger.getLogger(LuceneServer.class
												.getName());

	private Map<String, String>	knownFields;

	public LuceneQueryParser(Version version, Analyzer analyzer) {
		super(version, "an-unused-field", analyzer);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.lucene.queryParser.QueryParser#getRangeQuery(java.lang.String,
	 * java.lang.String, java.lang.String, boolean)
	 */
	@Override
	protected Query getRangeQuery(String field, String part1, String part2,
			boolean inclusive) throws ParseException {
		TermRangeQuery query = (TermRangeQuery) super.getRangeQuery(field,
				part1, part2, inclusive);
		String fieldClass = this.knownFields.get(field);
		try {
			if (fieldClass.equals(OtpErlangInt.class.getName())) {
				return NumericRangeQuery.newIntRange(field,
						(query.getLowerTerm() == null ? Integer.MIN_VALUE
								: Integer.parseInt(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Integer.MAX_VALUE
								: Integer.parseInt(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			} else if (fieldClass.equals(OtpErlangLong.class.getName())) {
				return NumericRangeQuery.newLongRange(field,
						(query.getLowerTerm() == null ? Long.MIN_VALUE
								: Long.parseLong(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Long.MAX_VALUE
								: Long.parseLong(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			} else if (fieldClass.equals(OtpErlangFloat.class.getName())) {
				return NumericRangeQuery.newFloatRange(field,
						(query.getLowerTerm() == null ? Float.MIN_VALUE
								: Float.parseFloat(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Float.MAX_VALUE
								: Float.parseFloat(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			} else if (fieldClass.equals(OtpErlangDouble.class.getName())) {
				return NumericRangeQuery.newDoubleRange(field,
						(query.getLowerTerm() == null ? Double.MIN_VALUE
								: Double.parseDouble(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Double.MAX_VALUE
								: Double.parseDouble(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			} else {
				return query;
			}
		} catch (NumberFormatException nfe) {
			jlog.info("Couldn't parse range: " + field + ":[" + part1 + " TO "
					+ part2 + "]");
			nfe.printStackTrace();
			return query;
		}
	}

	public Query parse(Map<String, String> knownFields, String queryString)
			throws ParseException {
		this.knownFields = knownFields;
		return super.parse(queryString);
	}
}