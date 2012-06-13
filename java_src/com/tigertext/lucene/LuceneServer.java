package com.tigertext.lucene;

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.ReusableAnalyzerBase;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.WhitespaceTokenizer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.Fieldable;
import org.apache.lucene.document.NumericField;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryParser.ParseException;
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
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpContinueException;
import com.ericsson.otp.stdlib.OtpGenServer;
import com.ericsson.otp.stdlib.OtpStopException;

public class LuceneServer extends OtpGenServer {
	private static final Logger	jlog	= Logger.getLogger(LuceneServer.class
												.getName());

	protected class UnsupportedFieldTypeException extends Exception {
		public UnsupportedFieldTypeException(String type) {
			super("Unsupported field type: " + type);
		}

		private static final long	serialVersionUID	= 8966657853257773634L;
	}

	protected class EndOfTableException extends Exception {
		private static final long	serialVersionUID	= 3118984031523050939L;
	}

	protected Analyzer				analyzer;
	protected Directory				index;
	protected IndexWriter			writer;
	protected LuceneQueryParser		queryParser;

	// TODO: Let the user configure the internal parameters (i.e. analyzer,
	// index, writer)
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
		this.queryParser = new LuceneQueryParser(Version.LUCENE_36, analyzer);
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
			try {
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						match(new LucenePageToken(queryString), pageSize) });
			} catch (EndOfTableException eote) {
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						new OtpErlangAtom("$end_of_table") });
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
			} catch (EndOfTableException e) {
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						new OtpErlangAtom("$end_of_table") });
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
				add(buildDocs(((OtpErlangList) cmdTuple.elementAt(1))
						.elements()));
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
			int pageSize) throws EndOfTableException, IOException,
			ParseException {
		LucenePageToken pageToken = (LucenePageToken) pageTokenAsObject;
		if (pageToken.isEmpty()) {
			throw new LuceneServer.EndOfTableException();
		} else {
			return match(pageToken, pageSize);
		}
	}

	private OtpErlangObject match(LucenePageToken pageToken, int pageSize)
			throws EndOfTableException, IOException, ParseException {
		IndexReader reader = IndexReader.open(this.index);

		Query q = this.queryParser.parse(pageToken.getQueryString());

		IndexSearcher searcher = new IndexSearcher(reader);
		TopScoreDocCollector collector = pageToken.getScoreDoc() == null ? TopScoreDocCollector
				.create(pageSize, true) : TopScoreDocCollector.create(pageSize,
				pageToken.getScoreDoc(), true);
		searcher.search(q, collector);
		TopDocs topDocs = collector.topDocs();
		ScoreDoc[] hits = topDocs.scoreDocs;

		int firstHit = 0;
		if (hits.length > 0) {
			firstHit = pageToken.getNextFirstHit();
		}

		List<Document> docs = new ArrayList<Document>(hits.length);
		for (ScoreDoc sd : hits) {
			docs.add(searcher.doc(sd.doc));
		}
		searcher.close();

		if (hits.length == pageSize) { // There may be a following page
			pageToken.setScoreDoc(hits[pageSize - 1]);
			pageToken.incrementFirstHit(pageSize);
		} else {
			pageToken = new LucenePageToken();
		}

		return encodeResult(docs, pageToken, topDocs.totalHits, firstHit);
	}

	private List<Document> buildDocs(OtpErlangObject[] objects)
			throws UnsupportedFieldTypeException {
		List<Document> docs = new ArrayList<Document>(objects.length);
		for (OtpErlangObject object : objects) {
			docs.add(buildDoc(((OtpErlangList) object).elements()));
		}
		return docs;
	}

	private Document buildDoc(OtpErlangObject[] props)
			throws UnsupportedFieldTypeException {
		Document doc = new Document();
		for (OtpErlangObject object : props) {
			OtpErlangTuple prop = (OtpErlangTuple) object;
			String key = ((OtpErlangAtom) prop.elementAt(0)).atomValue();
			if (prop.elementAt(1) instanceof OtpErlangString) {
				doc.add(new Field(key, ((OtpErlangString) prop.elementAt(1))
						.stringValue(), Field.Store.YES, Field.Index.ANALYZED));
				this.queryParser.putField(key, LuceneQueryParser.FieldType.STRING);
			} else if (prop.elementAt(1) instanceof OtpErlangList
					&& ((OtpErlangList) prop.elementAt(1)).arity() == 0) {
				doc.add(new Field(key, "", Field.Store.YES,
						Field.Index.ANALYZED));
				this.queryParser.putField(key, LuceneQueryParser.FieldType.STRING);
			} else if (prop.elementAt(1) instanceof OtpErlangInt) {
				NumericField field = new NumericField(key, Field.Store.YES,
						true);
				try {
					field.setIntValue(((OtpErlangInt) prop.elementAt(1))
							.intValue());
				} catch (OtpErlangRangeException e) {
					throw new UnsupportedFieldTypeException(prop.elementAt(1)
							.getClass().getName());
				}
				doc.add(field);
				this.queryParser.putField(key, LuceneQueryParser.FieldType.INT);
			} else if (prop.elementAt(1) instanceof OtpErlangLong) {
				NumericField field = new NumericField(key, Field.Store.YES,
						true);
				field.setLongValue(((OtpErlangLong) prop.elementAt(1))
						.longValue());
				doc.add(field);
				this.queryParser.putField(key, LuceneQueryParser.FieldType.LONG);
			} else if (prop.elementAt(1) instanceof OtpErlangFloat) {
				NumericField field = new NumericField(key, Field.Store.YES,
						true);
				try {
					field.setFloatValue(((OtpErlangFloat) prop.elementAt(1))
							.floatValue());
				} catch (OtpErlangRangeException e) {
					throw new UnsupportedFieldTypeException(prop.elementAt(1)
							.getClass().getName());
				}
				doc.add(field);
				this.queryParser.putField(key, LuceneQueryParser.FieldType.FLOAT);
			} else if (prop.elementAt(1) instanceof OtpErlangDouble) {
				NumericField field = new NumericField(key, Field.Store.YES,
						true);
				field.setDoubleValue(((OtpErlangDouble) prop.elementAt(1))
						.doubleValue());
				doc.add(field);
				this.queryParser.putField(key, LuceneQueryParser.FieldType.DOUBLE);
			} else {
				throw new UnsupportedFieldTypeException(prop.elementAt(1)
						.getClass().getName());
			}
		}
		return doc;
	}

	private OtpErlangObject encodeResult(List<Document> origValues,
			Serializable nextPage, int totalHits, int firstHit)
			throws IOException {
		jlog.entering(this.getClass().getName(), "encodeResult");

		// Values as a list of lists of tuples
		OtpErlangObject[] values = new OtpErlangObject[origValues.size()];
		for (int i = 0; i < origValues.size(); i++) {
			OtpErlangObject[] props = new OtpErlangObject[origValues.get(i)
					.getFields().size()];
			int j = 0;
			for (Fieldable field : origValues.get(i).getFields()) {
				OtpErlangAtom keyAsAtom = new OtpErlangAtom(field.name());
				OtpErlangString valueAsString = new OtpErlangString(
						field.stringValue());
				props[j] = new OtpErlangTuple(new OtpErlangObject[] {
						keyAsAtom, valueAsString });
				j++;
			}
			values[i] = new OtpErlangList(props);
		}
		OtpErlangList valuesAsList = new OtpErlangList(values);

		// Metadata as a proplist
		OtpErlangObject[] metadata = new OtpErlangObject[3];
		metadata[0] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("next_page"), new OtpErlangBinary(nextPage) });
		metadata[1] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("total_hits"), new OtpErlangLong(totalHits) });
		metadata[2] = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("first_hit"), new OtpErlangLong(firstHit) });
		OtpErlangList metadataAsList = new OtpErlangList(metadata);

		// Final result
		OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[] {
				valuesAsList, metadataAsList });
		jlog.exiting(this.getClass().getName(), "encodeResult", result);
		return result;
	}
}