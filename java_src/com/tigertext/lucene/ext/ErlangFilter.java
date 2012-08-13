package com.tigertext.lucene.ext;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.DocIdSet;
import org.apache.lucene.search.Filter;
import org.apache.lucene.search.FilteredDocIdSet;
import org.apache.lucene.util.FixedBitSet;
import org.apache.lucene.util.OpenBitSet;

import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangObject;

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

	/**
	 * Default constructor
	 * 
	 * @param mod
	 *            Erlang module
	 * @param fun
	 *            Erlang function (it must have arity = 1)
	 * @param fieldName
	 *            Lucene field to consider
	 */
	public ErlangFilter(String mod, String fun, String fieldName) {
		this.mod = mod;
		this.fun = fun;
		this.fieldName = fieldName;

		/* store calculated scores for reuse by other components */
		this.scores = new HashMap<Integer, Double>();
	}

	@Override
	public DocIdSet getDocIdSet(IndexReader reader) throws IOException {
		final FixedBitSet bits = new FixedBitSet(reader.maxDoc());

		final int docBase = this.nextDocBase;
		this.nextDocBase += reader.maxDoc();

		bits.flip(0, reader.maxDoc());
		
		return new FilteredDocIdSet(bits) {
			@Override
			protected boolean match(int docid) throws IOException {
				jlog.info("matching " + mod + ":" + fun + "(docs[" + docid
						+ "][" + fieldName + "]");

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