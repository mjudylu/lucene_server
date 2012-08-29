package com.tigertext.lucene;

import java.io.Serializable;

import org.apache.lucene.search.SortField;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Token that
 */
public final class LucenePageToken implements Serializable {
	private static final long	serialVersionUID	= -5595064630916761399L;

	private int					nextFirstHit;
	private String				queryString;

	private SortField[]			sortFields;

	/**
	 * Generates a token without scoreDoc, used as a reference to "first page"
	 * with a pre-built queryString
	 * 
	 * @param queryString
	 *            The base query
	 * @param sortFields
	 *            The fields used to sort the result
	 */
	public LucenePageToken(String queryString, SortField[] sortFields) {
		this.queryString = queryString;
		this.sortFields = new SortField[sortFields.length + 1];
		this.sortFields[0] = SortField.FIELD_SCORE;
		for (int i = 0; i < sortFields.length; i++)
			this.sortFields[i + 1] = sortFields[i];
		this.nextFirstHit = 1;
	}

	/**
	 * @return Number of the first hit of the next page
	 */
	public int getNextFirstHit() {
		return this.nextFirstHit;
	}

	/**
	 * @param increment
	 *            Increase the first hit for the next page
	 * @return The resulting first hit
	 */
	public int incrementFirstHit(int increment) {
		return this.nextFirstHit += increment;
	}

	@Override
	public String toString() {
		return "<token for " + this.queryString + ". Doc: " + this.nextFirstHit
				+ ">";
	}

	/**
	 * @return The base query string
	 */
	public String getQueryString() {
		return this.queryString;
	}

	/**
	 * @return The indication on how to sort stuff
	 */
	public SortField[] getSortFields() {
		return this.sortFields;
	}
}