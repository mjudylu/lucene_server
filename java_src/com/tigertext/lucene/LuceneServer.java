package com.tigertext.lucene;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.ReusableAnalyzerBase;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.WhitespaceTokenizer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.Fieldable;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TopScoreDocCollector;
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

public class LuceneServer extends OtpGenServer {
	private static final Logger jlog = Logger.getLogger(LuceneServer.class.getName());

	protected class EndOfTableException extends Exception {
		private static final long serialVersionUID = 3118984031523050939L;
	}

	protected class MatchResult {
		private List<Map<String, String>> values;
		private Serializable nextPage;
		private int firstHit;
		private int totalHits;

		public MatchResult() {
			this.values = new ArrayList<Map<String, String>>();
		}

		public List<Map<String, String>> getValues() {
			return values;
		}

		public void addValue(Map<String, String> value) {
			this.values.add(value);
		}

		public Serializable getNextPage() {
			return nextPage;
		}

		public void setNextPage(Serializable nextPage) {
			this.nextPage = nextPage;
		}

		@Override
		public String toString() {
			return "{" + values + " // " + nextPage + "}";
		}

		public void setFirstHit(int firstHit) {
			this.firstHit = firstHit;
		}

		public void setTotalHits(int totalHits) {
			this.totalHits = totalHits;
		}

		public int getFirstHit() {
			return this.firstHit;
		}

		public int getTotalHits() {
			return this.totalHits;
		}
	}

	Analyzer analyzer;
	Directory index;
	IndexWriter writer;

	//TODO: Let the user configure the internal parameters (i.e. analyzer, index, writer)
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
	}

	@Override
	protected OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException {

		OtpErlangTuple cmdTuple = (OtpErlangTuple) cmd;
		OtpErlangAtom cmdName = (OtpErlangAtom) cmdTuple.elementAt(0);
		if (cmdName.atomValue().equals("pid")) { // {pid}
			return super.getSelf();
		} else if (cmdName.atomValue().equals("match") && cmdTuple.arity() == 3) {
			// {match, Query :: string(), PageSize :: integer()}
			String queryString = ((OtpErlangString) cmdTuple.elementAt(1))
					.stringValue();
			int pageSize = ((OtpErlangLong) cmdTuple.elementAt(2)).intValue();
			try {
				return encodeResult(match(new LucenePageToken(queryString),
						pageSize));
			} catch (EndOfTableException eote) {
				return new OtpErlangAtom("$end_of_table");
			} catch (IOException e) {
				jlog.severe("Error trying to encode result: " + e);
				e.printStackTrace();
				return new OtpErlangAtom("$end_of_table");
			}
		} else if (cmdName.atomValue().equals("continue")) {
			// {continue, Token :: binary(), PageSize :: integer()}
			Object pageToken = ((OtpErlangBinary) cmdTuple.elementAt(1))
					.getObject();
			int pageSize = ((OtpErlangLong) cmdTuple.elementAt(2)).intValue();
			try {
				return encodeResult(continueMatch(pageToken, pageSize));
			} catch (EndOfTableException e) {
				return new OtpErlangAtom("$end_of_table");
			} catch (IOException e) {
				jlog.severe("Error trying to encode result: " + e);
				e.printStackTrace();
				return new OtpErlangAtom("$end_of_table");
			}
		} else {
			return new OtpErlangAtom("unknown command");
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
			// {add, Items :: [[{atom(), string()}]]}
			add(buildItems(((OtpErlangList) cmdTuple.elementAt(1)).elements()));
		}
	}

	@Override
	protected void handleInfo(OtpErlangObject cmd) throws OtpStopException {
	}

	@Override
	protected void handleExit(OtpErlangExit oee) throws OtpErlangExit,
			OtpStopException {
	}

	protected void add(List<Map<String, String>> items) {
		List<Document> docs = new ArrayList<Document>(items.size());
		for (Map<String, String> item : items) {
			Document doc = new Document();
			for (String key : item.keySet()) {
				//TODO: Let the user configure the field formats
				doc.add(new Field(key, item.get(key), Field.Store.YES,
						Field.Index.ANALYZED));
			}
			docs.add(doc);
		}
		try {
			this.writer.addDocuments(docs);
			this.writer.commit();
			jlog.info("" + items.size() + " docs added");
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
			QueryParser qp = new QueryParser(Version.LUCENE_36, "contents",
					this.analyzer);
			Query q = qp.parse(queryString);
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

	protected MatchResult continueMatch(Object pageTokenAsObject, int pageSize)
			throws EndOfTableException {
		LucenePageToken pageToken = (LucenePageToken) pageTokenAsObject;
		if (pageToken.isEmpty()) {
			throw new LuceneServer.EndOfTableException();
		} else {
			return match(pageToken, pageSize);
		}
	}

	private MatchResult match(LucenePageToken pageToken, int pageSize)
			throws EndOfTableException {
		try {
			IndexReader reader = IndexReader.open(this.index);

			Query q = pageToken.getQueryString(reader, this.analyzer);

			IndexSearcher searcher = new IndexSearcher(reader);
			TopScoreDocCollector collector = pageToken.getScoreDoc() == null ? TopScoreDocCollector
					.create(pageSize, true) : TopScoreDocCollector.create(
					pageSize, pageToken.getScoreDoc(), true);
			searcher.search(q, collector);
			TopDocs topDocs = collector.topDocs();
			ScoreDoc[] hits = topDocs.scoreDocs;

			MatchResult result = new MatchResult();
			result.setTotalHits(topDocs.totalHits);
			if (hits.length > 0) {
				result.setFirstHit(pageToken.getNextFirstHit());
			} else {
				result.setFirstHit(0);
			}

			for (ScoreDoc sd : hits) {
				Document d = searcher.doc(sd.doc);
				Map<String, String> props = new HashMap<String, String>(d
						.getFields().size());
				for (Fieldable field : d.getFields()) {
					props.put(field.name(), field.stringValue());
				}
				result.addValue(props);
			}
			searcher.close();

			if (hits.length == pageSize) { // There may be a following page
				pageToken.setScoreDoc(hits[pageSize - 1]);
				pageToken.incrementFirstHit(pageSize);
			} else {
				pageToken = new LucenePageToken();
			}
			result.setNextPage(pageToken);

			return result;
		} catch (IOException ioe) {
			jlog.severe("Couldn't search the index: " + ioe);
			ioe.printStackTrace();
			throw new EndOfTableException();
		} catch (ParseException pe) {
			jlog.severe("Bad Formatted Query");
			pe.printStackTrace();
			throw new EndOfTableException();
		}
	}

	private List<Map<String, String>> buildItems(OtpErlangObject[] objects) {
		List<Map<String, String>> items = new ArrayList<Map<String, String>>(
				objects.length);
		for (OtpErlangObject object : objects) {
			items.add(buildItem(((OtpErlangList) object).elements()));
		}
		return items;
	}

	private Map<String, String> buildItem(OtpErlangObject[] props) {
		Map<String, String> item = new HashMap<String, String>(props.length);
		for (OtpErlangObject object : props) {
			OtpErlangTuple prop = (OtpErlangTuple) object;
			String field = ((OtpErlangAtom) prop.elementAt(0)).atomValue();
			String value = "";
			if (prop.elementAt(1) instanceof OtpErlangString) {
				value = ((OtpErlangString) prop.elementAt(1)).stringValue();
			} // else it is a list []
			item.put(field, value);
		}
		return item;
	}

	/**
	 * Turns a MatchResult into {Values :: [ Value :: [{atom(), string()}]],
	 * NextPage :: binary()}
	 * 
	 * @param origResult
	 *            The original MatchResult
	 * @return the resulting Erlang tuple
	 * @throws IOException
	 *             if parsing goes wrong
	 */
	private OtpErlangObject encodeResult(MatchResult origResult)
			throws IOException {
		jlog.entering(this.getClass().getName(), "encodeResult", origResult);

		// Values as a list of lists of tuples
		List<Map<String, String>> origValues = origResult.getValues();
		OtpErlangObject[] values = new OtpErlangObject[origValues.size()];
		for (int i = 0; i < origValues.size(); i++) {
			OtpErlangObject[] props = new OtpErlangObject[origValues.get(i)
					.size()];
			int j = 0;
			for (String key : origValues.get(i).keySet()) {
				OtpErlangAtom keyAsAtom = new OtpErlangAtom(key);
				OtpErlangString valueAsString = new OtpErlangString(origValues
						.get(i).get(key));
				props[j] = new OtpErlangTuple(new OtpErlangObject[] {
						keyAsAtom, valueAsString });
				j++;
			}
			values[i] = new OtpErlangList(props);
		}
		OtpErlangList valuesAsList = new OtpErlangList(values);

		toByteArray(origResult.getNextPage());

		// Metadata as a proplist
		OtpErlangObject[] metadata = new OtpErlangObject[3];
		metadata[0] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("next_page"),
				new OtpErlangBinary(origResult.getNextPage()) });
		metadata[1] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("total_hits"),
				new OtpErlangLong(origResult.getTotalHits()) });
		metadata[2] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("first_hit"),
				new OtpErlangLong(origResult.getFirstHit()) });
		OtpErlangList metadataAsList = new OtpErlangList(metadata);

		// Final result
		OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[] {
				valuesAsList, metadataAsList });
		jlog.exiting(this.getClass().getName(), "encodeResult", result);
		return result;
	}

	private void toByteArray(final Object o) throws java.io.IOException {
		if (o == null)
			return;

		/* need to synchronize use of the shared baos */
		final java.io.ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(
				baos);

		oos.writeObject(o);
		oos.flush();
	}
}