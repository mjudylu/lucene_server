package com.tigertext.lucene.ext;

import java.io.IOException;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.function.DocValues;
import org.apache.lucene.search.function.ValueSource;

/**
 * {@link ValueSource} where values are taken from the execution of an Erlang
 * function
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class ErlangValueSource extends ValueSource {

	private static final long	serialVersionUID	= -8167987813348282785L;
	private ErlangFilter		filter;
	private int					nextDocBase;

	/**
	 * Default constructor
	 * 
	 * @param filter
	 *            Erlang filter from which to get scores
	 */
	public ErlangValueSource(ErlangFilter filter) {
		this.filter = filter;
	}

	@Override
	public String description() {
		return "Erlang scored by " + this.filter;
	}

	@Override
	public DocValues getValues(IndexReader reader) throws IOException {
		final int docBase = nextDocBase;
		nextDocBase += reader.maxDoc();
		return new DocValues() {

			@Override
			public double doubleVal(int doc) {
				return filter.getScore(doc + docBase);
			}

			@Override
			public String toString(int doc) {
				return description() + ":" + filter.getScore(doc + docBase);
			}

			@Override
			public float floatVal(int doc) {
				return filter.getScore(doc + docBase).floatValue();
			}
		};
	}

	@Override
	public boolean equals(Object o) {
		if (o == this)
			return true;
		if (o == null)
			return false;
		if (o.getClass() != ErlangValueSource.class)
			return false;
		ErlangValueSource other = (ErlangValueSource) o;
		return this.filter.equals(other.filter);
	}

	@Override
	public int hashCode() {
		return this.getClass().hashCode() + this.filter.hashCode();
	}
}
