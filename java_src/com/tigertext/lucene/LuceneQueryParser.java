package com.tigertext.lucene;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Fieldable;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.NumericRangeQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.util.Version;

public class LuceneQueryParser extends QueryParser {
	private static final Logger	jlog	= Logger.getLogger(LuceneServer.class
												.getName());

	public enum FieldType {
		STRING, INT, LONG, FLOAT, DOUBLE, GEO, ATOM
	}

	private Map<String, FieldType>	fields;

	public LuceneQueryParser(Version version, Analyzer analyzer) {
		super(version, "an-unused-field", analyzer);
		this.fields = new HashMap<String, LuceneQueryParser.FieldType>();
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
		try {
			switch (this.fields.get(field)) {
			case INT:
				return NumericRangeQuery.newIntRange(field,
						(query.getLowerTerm() == null ? Integer.MIN_VALUE
								: Integer.parseInt(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Integer.MAX_VALUE
								: Integer.parseInt(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			case LONG:
				return NumericRangeQuery.newLongRange(
						field,
						(query.getLowerTerm() == null ? Long.MIN_VALUE : Long
								.parseLong(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Long.MAX_VALUE : Long
								.parseLong(query.getUpperTerm())), query
								.includesLower(), query.includesUpper());
			case FLOAT:
				return NumericRangeQuery.newFloatRange(
						field,
						(query.getLowerTerm() == null ? Float.MIN_VALUE : Float
								.parseFloat(query.getLowerTerm())), (query
								.getUpperTerm() == null ? Float.MAX_VALUE
								: Float.parseFloat(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			case DOUBLE:
				return NumericRangeQuery.newDoubleRange(field,
						(query.getLowerTerm() == null ? Double.MIN_VALUE
								: Double.parseDouble(query.getLowerTerm())),
						(query.getUpperTerm() == null ? Double.MAX_VALUE
								: Double.parseDouble(query.getUpperTerm())),
						query.includesLower(), query.includesUpper());
			default:
				return query;
			}
		} catch (NullPointerException npe) {
			// An unknown field
			jlog.warning("Unknown field: " + field);
			return query;
		} catch (NumberFormatException nfe) {
			jlog.finer("Couldn't parse range: " + field + ":[" + part1 + " TO "
					+ part2 + "]");
			nfe.printStackTrace();
			return query;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.lucene.queryParser.QueryParser#getFieldQuery(java.lang.String,
	 * java.lang.String, boolean)
	 */
	@Override
	protected Query getFieldQuery(String field, String queryText, boolean quoted)
			throws ParseException {
		try {
			switch (this.fields.get(field)) {
			case INT:
			case LONG:
			case FLOAT:
			case DOUBLE:
				jlog.finer("Turning " + queryText + " into a range query for "
						+ field);
				return getRangeQuery(field, queryText, queryText, true);
			default:
				return super.getFieldQuery(field, queryText, quoted);
			}
		} catch (NullPointerException npe) {
			// An unknown field
			jlog.warning("Unknown field: " + field);
			return super.getFieldQuery(field, queryText, quoted);
		}
	}

	public void putField(String key, FieldType fieldType) {
		this.fields.put(key, fieldType);
	}

	public FieldType getFieldType(Fieldable field) {
		FieldType type = this.fields.get(field.name());
		if(type != null) {
			return type; 
		} else {
			return FieldType.STRING;
		}
	}
}