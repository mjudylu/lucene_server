package com.tigertext.lucene;

import java.io.Serializable;

import org.apache.lucene.search.ScoreDoc;

public final class LucenePageToken implements Serializable {
	private static final long serialVersionUID = -5595064630916761399L;

	private boolean empty;
	private ScoreDoc scoreDoc;
	private int nextFirstHit;
	private String queryString;

	/**
	 * Generates an empty page token, used to signal there're no more pages
	 */
	public LucenePageToken() {
		this.empty = true;
	}

	/**
	 * Generates a regular page token based on a query and document
	 */
	public LucenePageToken(ScoreDoc scoreDoc, String queryString) {
		this.empty = false;
		this.scoreDoc = scoreDoc;
		this.queryString = queryString;
		this.nextFirstHit = 1;
	}

	/**
	 * Generates a token without scoreDoc, used as a reference to "first page"
	 * with a pre-built queryString
	 */
	public LucenePageToken(String queryString) {
		this(null, queryString);
	}

	public boolean isEmpty() {
		return empty;
	}

	public ScoreDoc getScoreDoc() {
		return scoreDoc;
	}

	public void setScoreDoc(ScoreDoc scoreDoc) {
		this.scoreDoc = scoreDoc;
	}

	public int getNextFirstHit() {
		return this.nextFirstHit;
	}

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

	public String getQueryString() {
		return this.queryString;
	}
}