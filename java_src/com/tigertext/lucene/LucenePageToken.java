package com.tigertext.lucene;

import java.io.Serializable;

import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SortField;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Token that
 */
public final class LucenePageToken implements Serializable {
	private static final long	serialVersionUID	= -5595064630916761399L;

	private boolean				empty;
	private ScoreDoc			scoreDoc;
	private int					nextFirstHit;
	private String				queryString;

	private SortField[]			sortFields;

	/**
	 * Generates an empty page token, used to signal there're no more pages
	 */
	public LucenePageToken() {
		this.empty = true;
	}

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
		this.empty = false;
		this.scoreDoc = null;
		this.queryString = queryString;
		this.sortFields = sortFields;
		this.nextFirstHit = 1;
	}

	/**
	 * @return Is this an empty page?s
	 */
	public boolean isEmpty() {
		return empty;
	}

	/**
	 * @return The score doc to use by the TopScoreDocCollector
	 */
	public ScoreDoc getScoreDoc() {
		return scoreDoc;
	}

	/**
	 * @param scoreDoc
	 *            Sets the score doc to use by the TopScoreDocCollector
	 */
	public void setScoreDoc(ScoreDoc scoreDoc) {
		this.scoreDoc = scoreDoc;
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
	 */
	public void incrementFirstHit(int increment) {
		this.nextFirstHit += increment;
	}

	@Override
	public String toString() {
		if (this.empty) {
			return "<emtpy token>";
		} else {
			return "<token for " + this.queryString + ". Doc: "
					+ (this.scoreDoc == null ? "none" : this.scoreDoc) + ">";
		}
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