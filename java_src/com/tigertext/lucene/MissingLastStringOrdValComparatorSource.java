package com.tigertext.lucene;

import java.io.IOException;

import org.apache.lucene.search.FieldComparator;
import org.apache.lucene.search.FieldComparatorSource;

/**
 * Source for {@link MissingLastStringOrdValComparator}
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class MissingLastStringOrdValComparatorSource extends
		FieldComparatorSource {

	/**
	 * for serialization
	 */
	private static final long	serialVersionUID	= -8156968111777826061L;

	@Override
	public FieldComparator<?> newComparator(String fieldname, int numHits,
			int sortPos, boolean reversed) throws IOException {
		return new MissingLastStringOrdValComparator(numHits, fieldname,
				sortPos, reversed);
	}

}