package com.tigertext.lucene;

import java.io.IOException;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.FieldComparator;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Works like
 *         {@link org.apache.lucene.search.FieldComparator.StringValComparator}
 *         but it sorts null values at the end
 */
public class MissingLastStringOrdValComparator extends FieldComparator<String> {

	private StringOrdValComparator	baseComparator;

	/**
	 * @param numHits
	 *            How many documents to return
	 * @param field
	 *            Field to sort docs
	 * @param sortPos
	 *            Staring doc
	 * @param reverse
	 *            In reverse order?
	 */
	public MissingLastStringOrdValComparator(int numHits, String field,
			int sortPos, boolean reverse) {
		this.baseComparator = new StringOrdValComparator(numHits, field,
				sortPos, reverse);
	}

	@Override
	public int compare(int slot1, int slot2) {
		if (this.baseComparator.value(slot1) == null) {
			if (this.baseComparator.value(slot2) == null) {
				return 0; // value[slot1] == value[slot2]
			} else {
				return 1; // value[slot2] < value[slot1]
			}
		} else if (this.baseComparator.value(slot2) == null) {
			return -1; // value[slot1] < value[slot2]
		}

		return this.baseComparator.compare(slot1, slot2);
	}

	@Override
	public void setBottom(int slot) {
		this.baseComparator.setBottom(slot);
	}

	@Override
	public int compareBottom(int doc) throws IOException {
		return this.baseComparator.compareBottom(doc);
	}

	@Override
	public void copy(int slot, int doc) throws IOException {
		this.baseComparator.copy(slot, doc);
	}

	@Override
	public void setNextReader(IndexReader reader, int docBase)
			throws IOException {
		this.baseComparator.setNextReader(reader, docBase);
	}

	@Override
	public String value(int slot) {
		return this.baseComparator.value(slot);
	}

}
