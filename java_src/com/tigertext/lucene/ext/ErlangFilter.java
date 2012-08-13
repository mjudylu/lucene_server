package com.tigertext.lucene.ext;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.DocIdSet;
import org.apache.lucene.search.FieldCache;
import org.apache.lucene.search.Filter;
import org.apache.lucene.search.FilteredDocIdSet;
import org.apache.lucene.util.FixedBitSet;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.tigertext.lucene.DocumentTranslator;
import com.tigertext.lucene.DocumentTranslator.FieldType;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Erlang filter: filters
 *         results according to an erlang module and function and keeps them
 *         cached for further use
 */
public class ErlangFilter extends Filter {
	private static final Logger		jlog				= Logger.getLogger(ErlangFilter.class
																.getName());

	private static final long		serialVersionUID	= 892972394357766013L;
	private final String			mod;
	private final String			fun;
	private final String			fieldName;
	private Map<Integer, Double>	scores;
	private int						nextDocBase;

	private FieldType				fieldType;

	/**
	 * Default constructor
	 * 
	 * @param mod
	 *            Erlang module
	 * @param fun
	 *            Erlang function (it must have arity = 1)
	 * @param fieldName
	 *            Lucene field to consider
	 * @param fieldType
	 *            Type of the values to find in fieldName
	 */
	public ErlangFilter(String mod, String fun, String fieldName,
			FieldType fieldType) {
		this.mod = mod;
		this.fun = fun;
		this.fieldName = fieldName;
		this.fieldType = fieldType;

		/* store calculated scores for reuse by other components */
		this.scores = new HashMap<Integer, Double>();
	}

	@Override
	public DocIdSet getDocIdSet(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;

		switch (this.fieldType) {
		case ATOM:
			docValues = getAtoms(reader);
			break;
		case DOUBLE:
			docValues = getDoubles(reader);
			break;
		case FLOAT:
			docValues = getFloats(reader);
			break;
		case GEO:
			docValues = getGeos(reader);
			break;
		case INT:
			docValues = getInts(reader);
			break;
		case LONG:
			docValues = getLongs(reader);
			break;
		default:
			docValues = getStrings(reader);
			break;
		}

		final int docBase = this.nextDocBase;
		this.nextDocBase += reader.maxDoc();

		final FixedBitSet bits = new FixedBitSet(reader.maxDoc());
		bits.flip(0, reader.maxDoc());

		return new FilteredDocIdSet(bits) {
			@Override
			protected boolean match(int docid) throws IOException {
				jlog.info("matching " + mod + ":" + fun + "(docs[" + docid
						+ "][" + fieldName + "] = " + docValues[docid] + ")");

				OtpErlangObject result = new OtpErlangDouble(1.5);

				if (result instanceof OtpErlangDouble) {
					scores.put(docid + docBase,
							((OtpErlangDouble) result).doubleValue());
					return true;
				} else {
					return false;
				}
			}
		};
	}

	private OtpErlangObject[] getGeos(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		final double[] latIndex = FieldCache.DEFAULT.getDoubles(reader,
				this.fieldName + "`lat");
		final double[] lngIndex = FieldCache.DEFAULT.getDoubles(reader,
				this.fieldName + "`lng");
		docValues = new OtpErlangObject[latIndex.length];
		for (int i = 0; i < latIndex.length; i++) {
			docValues[i] = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("geo"), new OtpErlangDouble(latIndex[i]),
					new OtpErlangDouble(lngIndex[i]) });
		}
		return docValues;
	}

	private OtpErlangObject[] getStrings(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		String[] origs = FieldCache.DEFAULT.getStrings(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangString(origs[i]);
		}
		return docValues;
	}

	private OtpErlangObject[] getLongs(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		long[] origs = FieldCache.DEFAULT.getLongs(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangLong(origs[i]);
		}
		return docValues;
	}

	private OtpErlangObject[] getInts(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		int[] origs = FieldCache.DEFAULT.getInts(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangInt(origs[i]);
		}
		return docValues;
	}

	private OtpErlangObject[] getFloats(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		float[] origs = FieldCache.DEFAULT.getFloats(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangFloat(origs[i]);
		}
		return docValues;
	}

	private OtpErlangObject[] getDoubles(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		double[] origs = FieldCache.DEFAULT.getDoubles(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangDouble(origs[i]);
		}
		return docValues;
	}

	protected OtpErlangObject[] getAtoms(IndexReader reader) throws IOException {
		final OtpErlangObject[] docValues;
		String[] origs = FieldCache.DEFAULT.getStrings(reader, this.fieldName);
		docValues = new OtpErlangObject[origs.length];
		for (int i = 0; i < origs.length; i++) {
			docValues[i] = new OtpErlangAtom(origs[i]);
		}
		return docValues;
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
		return this.mod + ":" + this.fun + "(doc." + this.fieldName + ")";
	}
}