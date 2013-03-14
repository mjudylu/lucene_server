package com.tigertext.lucene;

import java.io.File;
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
import org.apache.lucene.index.IndexFileNameFilter;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.queryParser.ext.Extensions;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TopFieldCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
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

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * 
 */
public class LuceneServer extends OtpGenServer {
	private static final Logger		jlog	= Logger.getLogger(LuceneServer.class
													.getName());

	protected Analyzer				analyzer;
	protected Directory				index;
	protected IndexWriter			writer;
	protected DocumentTranslator	translator;
	protected Extensions			extensions;

	private int						allowedThreads;

	private int						initialThreads;

	private SearcherManager			searcherManager;

	// TODO: Let the user configure the internal parameters (i.e. analyzer,
	// index, writer)
	/**
	 * @param host
	 *            Host node
	 * @param allowedThreads
	 *            Number of threads allowed to run queries at the same time
	 * @throws CorruptIndexException
	 *             Check Lucene docs for a description
	 * @throws LockObtainFailedException
	 *             Check Lucene docs for a description
	 * @throws IOException
	 *             Check Lucene docs for a description
	 */
	public LuceneServer(OtpNode host, int allowedThreads)
			throws CorruptIndexException, LockObtainFailedException,
			IOException {
		super(host, "lucene_server");

		this.allowedThreads = allowedThreads;
		this.initialThreads = Thread.activeCount() + 3;

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
		this.extensions = ext;
		try {
			add(this.translator.convert(new OtpErlangList(new OtpErlangList(
					new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("to"),
							new OtpErlangAtom("delete") })))));
			del("to:delete");
		} catch (UnsupportedFieldTypeException e) {
		}
		final SearcherManager searcherManager = new SearcherManager(
				this.writer, true, null);
		this.searcherManager = searcherManager;
		new Thread("SearcherManager-refresher") {
			@Override
			public void run() {
				try {
					while (true) {
						searcherManager.maybeRefresh();
						sleep(10000);
					}
				} catch (IOException ioe) {
					jlog.severe("Couldn't refresh searcher:\n\t" + ioe);
					ioe.printStackTrace();
				} catch (InterruptedException e) {
					return;
				}
			};
		}.start();
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
			runMatch(queryString, pageSize, sortFields, from);
			throw new OtpContinueException();

		} else if (cmdName.atomValue().equals("continue")) {
			// {continue, Token :: binary(), PageSize :: integer()}
			Object pageToken = ((OtpErlangBinary) cmdTuple.elementAt(1))
					.getObject();
			int pageSize = ((OtpErlangLong) cmdTuple.elementAt(2)).intValue();
			runContinue(pageToken, pageSize, from);
			throw new OtpContinueException();

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
		if (cmdName.equals("save")) { // {save, Path}
			save(((OtpErlangString) cmdTuple.elementAt(1)).stringValue());
		} else if (cmdName.equals("clear")) { // {clear}
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

	private void save(String dir) {
		File folder = new File(dir);
		try {
			Directory dest = FSDirectory.open(folder);
			IndexFileNameFilter filter = IndexFileNameFilter.getFilter();
			for (String file : this.index.listAll()) {
				if (filter.accept(null, file)) {
					this.index.copy(dest, file, file);
				}
			}
		} catch (IOException ioe) {
			jlog.severe("Couldn't copy index:\n\t" + ioe);
			ioe.printStackTrace();
		}
	}

	@Override
	protected void handleInfo(OtpErlangObject cmd) throws OtpStopException {
	}

	@Override
	protected void handleExit(OtpErlangExit oee) throws OtpErlangExit,
			OtpStopException {
	}

	private QueryParser queryParser() {
		return new LuceneQueryParser(Version.LUCENE_36, this.analyzer,
				this.translator, this.extensions);
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
			Query q = this.queryParser().parse(queryString);
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
			int pageSize) throws IOException, ParseException {
		LucenePageToken pageToken = (LucenePageToken) pageTokenAsObject;
		return match(pageToken, pageSize);
	}

	private OtpErlangObject match(LucenePageToken pageToken, int pageSize)
			throws IOException, ParseException {

		long t0 = System.nanoTime();

		this.searcherManager.maybeRefresh();
		IndexSearcher searcher = this.searcherManager.acquire();

		Query q = this.queryParser().parse(pageToken.getQueryString());
		TopDocs topDocs;

		Sort sort = new Sort(pageToken.getSortFields());
		TopFieldCollector collector = TopFieldCollector.create(sort,
				pageToken.getNextFirstHit() + pageSize - 1, true, true, true,
				true);

		long t1 = System.nanoTime();
		searcher.search(q, collector);
		long t2 = System.nanoTime();

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
		searcherManager.release(searcher);
		searcher = null;

		boolean nextPage = hits.length == pageSize
				&& pageToken.incrementFirstHit(pageSize) <= topDocs.totalHits;

		OtpErlangList valuesAsList = this.translator.convert(docs, hits);

		long t3 = System.nanoTime();

		long queryTime = (t3 - t0) / 1000;
		long searchTime = (t2 - t1) / 1000;

		// Metadata as a proplist

		OtpErlangObject[] metadata = new OtpErlangObject[nextPage ? 5 : 4];
		metadata[0] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("total_hits"),
				new OtpErlangLong(topDocs.totalHits) });
		metadata[1] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("first_hit"), new OtpErlangLong(firstHit) });
		metadata[2] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("query_time"), new OtpErlangLong(queryTime) });
		metadata[3] = new OtpErlangTuple(
				new OtpErlangObject[] { new OtpErlangAtom("search_time"),
						new OtpErlangLong(searchTime) });
		if (nextPage) {
			metadata[4] = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("next_page"),
					new OtpErlangBinary(pageToken) });
		}

		OtpErlangList metadataAsList = new OtpErlangList(metadata);

		// Final result
		return new OtpErlangTuple(new OtpErlangObject[] { valuesAsList,
				metadataAsList });
	}

	/**
	 * @param pageToken
	 *            From where to continue
	 * @param pageSize
	 *            Number of results per page
	 * @param from
	 *            Process expecting the response
	 */
	protected void runContinue(final Object pageToken, final int pageSize,
			final OtpErlangTuple from) {
		new Thread("continue-runner") {
			@Override
			public void run() {
				OtpErlangObject reply = null;
				try {
					reply = new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("ok"),
							continueMatch(pageToken, pageSize) });
				} catch (IOException ioe) {
					jlog.severe("Couldn't search the index: " + ioe);
					ioe.printStackTrace();
					reply = new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("error"),
							new OtpErlangString(ioe.getMessage()) });
				} catch (ParseException pe) {
					jlog.severe("Bad Formatted Query");
					pe.printStackTrace();
					reply = new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("error"),
							new OtpErlangString(pe.getMessage()) });
				}
				OtpGenServer.reply(LuceneNode.NODE, from, reply);
			}
		}.start();
	}

	/**
	 * @param queryString
	 *            Query to match indexed docs against
	 * @param sortFields
	 *            Fields to sort the result
	 * @param pageSize
	 *            Number of results per page
	 * @param from
	 *            Process expecting the response
	 */
	protected void runMatch(final String queryString, final int pageSize,
			final SortField[] sortFields, final OtpErlangTuple from) {
		int threadCount = Thread.activeCount() - this.initialThreads;
		jlog.info("Currently using " + threadCount + " threads");
		if (threadCount <= this.allowedThreads) {
			new Thread("query-runner") {
				@Override
				public void run() {
					doRunMatch(queryString, pageSize, sortFields, from);
				}
			}.start();
		} else {
			jlog.warning("More than " + this.allowedThreads
					+ " threads... waiting...");
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
			}
			runMatch(queryString, pageSize, sortFields, from);
		}
	}

	/**
	 * @param queryString
	 *            Query to match indexed docs against
	 * @param sortFields
	 *            Fields to sort the result
	 * @param pageSize
	 *            Number of results per page
	 * @param from
	 *            Process expecting the response
	 */
	protected void doRunMatch(final String queryString, final int pageSize,
			final SortField[] sortFields, final OtpErlangTuple from) {
		OtpErlangObject reply = null;
		try {
			reply = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("ok"),
					match(new LucenePageToken(queryString, sortFields),
							pageSize) });
		} catch (IOException ioe) {
			jlog.severe("Couldn't search the index: " + ioe);
			ioe.printStackTrace();
			reply = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(ioe.getMessage()) });
		} catch (ParseException pe) {
			jlog.severe("Bad Formatted Query");
			pe.printStackTrace();
			reply = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(pe.getMessage()) });
		}
		OtpGenServer.reply(LuceneNode.NODE, from, reply);
	}
}
