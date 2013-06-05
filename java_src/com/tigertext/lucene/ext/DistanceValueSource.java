package com.tigertext.lucene.ext;

import java.io.IOException;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.function.DocValues;
import org.apache.lucene.search.function.ValueSource;
import org.apache.lucene.spatial.tier.DistanceFilter;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 *
 */
@SuppressWarnings("deprecation")
public class DistanceValueSource extends ValueSource {
	private static final int	hcode				= DistanceValueSource.class
															.hashCode();
	private static final long	serialVersionUID	= -6171952145451039463L;
	private DistanceFilter		filter;
	protected int				nextDocBase;

	/**
	 * @param filter Filter to apply to values
	 */
	public DistanceValueSource(DistanceFilter filter) {
		this.filter = filter;
	}

	@Override
	public String description() {
		return "distance measured by " + this.filter;
	}

	@Override
	public DocValues getValues(IndexReader reader) throws IOException {
		final int docBase = nextDocBase;
		nextDocBase += reader.maxDoc();
		return new DocValues() {

			@Override
			public double doubleVal(int doc) {
				return filter.getDistance(doc + docBase);
			}

			@Override
			public String toString(int doc) {
				return description() + ":" + filter.getDistance(doc + docBase);
			}

			@Override
			public float floatVal(int doc) {
				return (-1) * filter.getDistance(doc + docBase).floatValue();
			}
		};
	}

	@Override
	public boolean equals(Object o) {
		if (o == this)
			return true;
		if (o == null)
			return false;
		if (o.getClass() != DistanceValueSource.class)
			return false;
		DistanceValueSource other = (DistanceValueSource) o;
		return this.filter.equals(other.filter);
	}

	@Override
	public int hashCode() {
		return hcode + this.filter.hashCode();
	}
}
