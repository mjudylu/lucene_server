package com.tigertext.lucene;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.ReusableAnalyzerBase;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.WhitespaceTokenizer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.queryParser.ext.Extensions;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TopFieldCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.LockObtainFailedException;
import org.apache.lucene.store.RAMDirectory;
import org.apache.lucene.util.Version;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpContinueException;
import com.ericsson.otp.stdlib.OtpGenServer;
import com.ericsson.otp.stdlib.OtpStopException;
import com.tigertext.lucene.DocumentTranslator.UnsupportedFieldTypeException;
import com.tigertext.lucene.ext.ErlangParserExtension;
import com.tigertext.lucene.ext.NearParserExtension;

public class LuceneServer extends OtpGenServer {
	private static final Logger	jlog	= Logger.getLogger(LuceneServer.class
												.getName());

	protected Analyzer				analyzer;
	protected Directory				index;
	protected IndexWriter			writer;
	protected QueryParser			queryParser;
	protected DocumentTranslator	translator;

	// TODO: Let the user configure the internal parameters (i.e. analyzer,
	// index, writer)
	/**
	 * @param host
	 * @throws CorruptIndexException
	 * @throws LockObtainFailedException
	 * @throws IOException
	 */
	public LuceneServer(OtpNode host) throws CorruptIndexException,
			LockObtainFailedException, IOException {
		super(host, "lucene_server");

		this.analyzer = new ReusableAnalyzerBase() {
			@Override
			protected TokenStreamComponents createComponents(String fieldName,
					Reader reader) {
				WhitespaceTokenizer tokenStream = new WhitespaceTokenizer(
						Version.LUCENE_36, reader);
				TokenStream result = new LowerCaseFilter(Version.LUCENE_36,
						tokenStream);
				return new TokenStreamComponents(tokenStream, result);
			}
		};
		this.index = new RAMDirectory();
		this.writer = new IndexWriter(this.index, new IndexWriterConfig(
				Version.LUCENE_36, this.analyzer));
		this.translator = new DocumentTranslator();
		Extensions ext = new Extensions('.');
		ext.add("near", new NearParserExtension());
		ext.add("erlang", new ErlangParserExtension(this.translator));
		this.queryParser = new LuceneQueryParser(Version.LUCENE_36,
				this.analyzer, this.translator, ext);
		try {
			add(this.translator
					.convert(new OtpErlangList(new OtpErlangList(
							new OtpErlangTuple(new OtpErlangObject[] {
									new OtpErlangAtom("to"),
									new OtpErlangAtom("delete") })))));
			del("to:delete");
		} catch (UnsupportedFieldTypeException e) {
		}
	}

	@Override
	protected OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException {

		OtpErlangTuple cmdTuple = (OtpErlangTuple) cmd;
		OtpErlangAtom cmdName = (OtpErlangAtom) cmdTuple.elementAt(0);
		if (cmdName.atomValue().equals("pid")) { // {pid}
			return super.getSelf();

		} else if (cmdName.atomValue().equals("match")) {
			// {match, Query :: string(), PageSize :: integer()}
			String queryString = ((OtpErlangString) cmdTuple.elementAt(1))
					.stringValue();
			int pageSize = ((OtpErlangLong) cmdTuple.elementAt(2)).intValue();
			OtpErlangObject[] sortFieldNames = ((OtpErlangList) cmdTuple
					.elementAt(3)).elements();
			SortField[] sortFields = new SortField[sortFieldNames.length];
			for (int i = 0; i < sortFields.length; i++) {
				sortFields[i] = this.translator
						.createSortField((OtpErlangAtom) sortFieldNames[i]);
			}
			try {
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						match(new LucenePageToken(queryString, sortFields),
								pageSize) });
			} catch (IOException ioe) {
				jlog.severe("Couldn't search the index: " + ioe);
				ioe.printStackTrace();
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(ioe.getMessage()) });
			} catch (ParseException pe) {
				jlog.severe("Bad Formatted Query");
				pe.printStackTrace();
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(pe.getMessage()) });
			}

		} else if (cmdName.atomValue().equals("continue")) {
			// {continue, Token :: binary(), PageSize :: integer()}
			Object pageToken = ((OtpErlangBinary) cmdTuple.elementAt(1))
					.getObject();
			int pageSize = ((OtpErlangLong) cmdTuple.elementAt(2)).intValue();
			try {
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						continueMatch(pageToken, pageSize) });
			} catch (IOException ioe) {
				jlog.severe("Couldn't search the index: " + ioe);
				ioe.printStackTrace();
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(ioe.getMessage()) });
			} catch (ParseException pe) {
				jlog.severe("Bad Formatted Query");
				pe.printStackTrace();
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(pe.getMessage()) });
			}

		} else {
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangAtom("unknown command") });
		}
	}

	@Override
	protected void handleCast(OtpErlangObject cmd) throws OtpStopException,
			OtpErlangException {
		OtpErlangTuple cmdTuple = (OtpErlangTuple) cmd;
		String cmdName = ((OtpErlangAtom) cmdTuple.elementAt(0)).atomValue();
		if (cmdName.equals("clear")) { // {clear}
			clear();
		} else if (cmdName.equals("del")) {
			// {del, Query :: string()}
			del(((OtpErlangString) cmdTuple.elementAt(1)).stringValue());
		} else if (cmdName.equals("add")) {
			// {add, Docs :: [[{atom(), string()}]]}
			try {
				add(this.translator.convert((OtpErlangList) cmdTuple
						.elementAt(1)));
			} catch (UnsupportedFieldTypeException ufte) {
				jlog.severe(ufte.getMessage());
				ufte.printStackTrace();
			}
		} else if (cmdName.equals("stop")) { // {stop}
			jlog.info("Stopping");
			throw new OtpStopException();
		}
	}

	@Override
	protected void handleInfo(OtpErlangObject cmd) throws OtpStopException {
	}

	@Override
	protected void handleExit(OtpErlangExit oee) throws OtpErlangExit,
			OtpStopException {
	}

	protected void add(List<Document> docs) {
		try {
			this.writer.addDocuments(docs);
			this.writer.commit();
			jlog.info("" + docs.size() + " docs added");
		} catch (CorruptIndexException cie) {
			jlog.severe("Corrupt index!");
			cie.printStackTrace();
		} catch (IOException ioe) {
			jlog.severe("Couldn't write docs:\n\t" + ioe);
			ioe.printStackTrace();
		}
	}

	private void del(String queryString) {
		jlog.info("Deleting " + queryString);
		try {
			Query q = this.queryParser.parse(queryString);
			this.writer.deleteDocuments(q);
			this.writer.commit();
			jlog.info("Several docs deleted");
		} catch (CorruptIndexException cie) {
			jlog.severe("Corrupt index!");
			cie.printStackTrace();
		} catch (IOException ioe) {
			jlog.severe("Couldn't del values docs:\n\t" + ioe);
			ioe.printStackTrace();
		} catch (ParseException pe) {
			jlog.severe("Couldn't parse del query:\n\t" + pe);
			pe.printStackTrace();
		}
	}

	protected void clear() {
		try {
			this.writer.deleteAll();
			this.writer.commit();
			jlog.info("All docs deleted");
		} catch (IOException ioe) {
			jlog.severe("Couldn't del all values:\n\t" + ioe);
			ioe.printStackTrace();
		}
	}

	protected OtpErlangObject continueMatch(Object pageTokenAsObject,
			int pageSize) throws IOException,
			ParseException {
		LucenePageToken pageToken = (LucenePageToken) pageTokenAsObject;
		return match(pageToken, pageSize);
	}

	private OtpErlangObject match(LucenePageToken pageToken, int pageSize)
			throws IOException, ParseException {
		IndexReader reader = IndexReader.open(this.index);

		Query q = this.queryParser.parse(pageToken.getQueryString());

		IndexSearcher searcher = new IndexSearcher(reader);
		TopDocs topDocs;

		Sort sort = new Sort(pageToken.getSortFields());
		TopFieldCollector collector = TopFieldCollector.create(sort,
				pageToken.getNextFirstHit() + pageSize - 1, true, true, true,
				true);
		searcher.search(q, collector);
		topDocs = collector.topDocs(pageToken.getNextFirstHit() - 1);

		ScoreDoc[] hits = topDocs.scoreDocs;
		// jlog.info("Sort: " + sort + "; topDocs: " + topDocs + "; hits: + " +
		// hits);
		int firstHit = 0;
		if (hits.length > 0) {
			firstHit = pageToken.getNextFirstHit();
		}

		List<Document> docs = new ArrayList<Document>(hits.length);
		for (ScoreDoc sd : hits) {
			docs.add(searcher.doc(sd.doc));
		}
		searcher.close();

		boolean nextPage = hits.length == pageSize
				&& pageToken.incrementFirstHit(pageSize) <= topDocs.totalHits;

		OtpErlangList valuesAsList = this.translator.convert(docs, hits);

		// Metadata as a proplist

		OtpErlangObject[] metadata = new OtpErlangObject[nextPage ? 3 : 2];
		metadata[0] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("total_hits"),
				new OtpErlangLong(topDocs.totalHits) });
		metadata[1] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("first_hit"), new OtpErlangLong(firstHit) });
		if (nextPage) {
			metadata[2] = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("next_page"),
					new OtpErlangBinary(pageToken) });
		}

		OtpErlangList metadataAsList = new OtpErlangList(metadata);

		// Final result
		return new OtpErlangTuple(new OtpErlangObject[] { valuesAsList,
				metadataAsList });
	}
}
