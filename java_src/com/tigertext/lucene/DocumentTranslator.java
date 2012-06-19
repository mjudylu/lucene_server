package com.tigertext.lucene;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.Fieldable;
import org.apache.lucene.document.NumericField;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DocumentTranslator {
	private static final Logger	jlog			= Logger.getLogger(LuceneServer.class
														.getName());

	private static final String	GEO_SEPARATOR	= "/";

	private LuceneQueryParser	queryParser;

	public class UnsupportedFieldTypeException extends Exception {
		public UnsupportedFieldTypeException(
				Class<? extends OtpErlangObject> class1) {
			super("Unsupported field type: " + class1);
		}

		private static final long	serialVersionUID	= 8966657853257773634L;
	}

	public DocumentTranslator(LuceneQueryParser queryParser) {
		this.queryParser = queryParser;
	}

	protected OtpErlangList convert(List<Document> docs) {
		OtpErlangObject[] values = new OtpErlangObject[docs.size()];
		for (int i = 0; i < docs.size(); i++) {
			OtpErlangObject[] props = new OtpErlangObject[docs.get(i)
					.getFields().size()];
			int j = 0;
			for (Fieldable field : docs.get(i).getFields()) {
				OtpErlangAtom key = new OtpErlangAtom(field.name());
				OtpErlangObject value = parseField(field);
				props[j] = new OtpErlangTuple(new OtpErlangObject[] { key,
						value });
				j++;
			}
			values[i] = new OtpErlangList(props);
		}
		OtpErlangList valuesAsList = new OtpErlangList(values);
		return valuesAsList;
	}

	private OtpErlangObject parseField(Fieldable field) {
		switch (this.queryParser.getFieldType(field)) {
		case DOUBLE:
			return new OtpErlangDouble(Double.parseDouble(field.stringValue()));
		case FLOAT:
			return new OtpErlangFloat(Float.parseFloat(field.stringValue()));
		case INT:
			return new OtpErlangInt(Integer.parseInt(field.stringValue()));
		case LONG:
			return new OtpErlangLong(Long.parseLong(field.stringValue()));
		case ATOM:
			return new OtpErlangAtom(field.stringValue());
		case GEO:
			String[] latLong = field.stringValue().split(GEO_SEPARATOR);
			if (latLong.length != 2)
				return new OtpErlangString(field.stringValue());
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("geo"),
					new OtpErlangDouble(Double.parseDouble(latLong[0])),
					new OtpErlangDouble(Double.parseDouble(latLong[1])) });
		default:
			return new OtpErlangString(field.stringValue());
		}
	}

	public List<Document> convert(OtpErlangList objects)
			throws UnsupportedFieldTypeException {
		List<Document> docs = new ArrayList<Document>(objects.arity());
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
			OtpErlangObject value = prop.elementAt(1);

			addField(doc, key, value);
		}
		return doc;
	}

	protected void addField(Document doc, String key, OtpErlangObject value)
			throws UnsupportedFieldTypeException {
		try {
			this.getClass()
					.getDeclaredMethod("addField", Document.class,
							String.class, value.getClass())
					.invoke(this, doc, key, value);
		} catch (InvocationTargetException ite) {
			if (ite.getTargetException() instanceof UnsupportedFieldTypeException) {
				throw (UnsupportedFieldTypeException) ite.getTargetException();
			} else {
				jlog.severe("Invoke problems: " + ite);
				ite.printStackTrace();
				throw new UnsupportedFieldTypeException(value.getClass());
			}
		} catch (NoSuchMethodException e) {
			throw new UnsupportedFieldTypeException(value.getClass());
		} catch (Exception e) {
			jlog.severe("Invoke problems: " + e);
			e.printStackTrace();
			throw new UnsupportedFieldTypeException(value.getClass());
		}
	}

	public void addField(Document doc, String key, OtpErlangTuple value)
			throws UnsupportedFieldTypeException {
		if (value.arity() == 3
				&& value.elementAt(0) instanceof OtpErlangAtom
				&& ((OtpErlangAtom) value.elementAt(0)).atomValue().equals(
						"geo")) {
			addField(doc, key + "`lat", value.elementAt(1));
			addField(doc, key + "`long", value.elementAt(2));
			String stringValue = Double.toString(((OtpErlangDouble) value
					.elementAt(1)).doubleValue())
					+ GEO_SEPARATOR
					+ Double.toString(((OtpErlangDouble) value.elementAt(2))
							.doubleValue());
			doc.add(new Field(key, stringValue, Field.Store.YES,
					Field.Index.ANALYZED));
			this.queryParser.putField(key, LuceneQueryParser.FieldType.GEO);

		} else {
			throw new UnsupportedFieldTypeException(value.getClass());
		}
	}

	public void addField(Document doc, String key, OtpErlangDouble value) {
		NumericField field = new NumericField(key, Field.Store.YES, true);
		field.setDoubleValue(value.doubleValue());
		doc.add(field);
		this.queryParser.putField(key, LuceneQueryParser.FieldType.DOUBLE);
	}

	public void addField(Document doc, String key, OtpErlangFloat value)
			throws UnsupportedFieldTypeException {
		NumericField field = new NumericField(key, Field.Store.YES, true);
		try {
			field.setFloatValue(value.floatValue());
		} catch (OtpErlangRangeException e) {
			throw new UnsupportedFieldTypeException(value.getClass());
		}
		doc.add(field);
		this.queryParser.putField(key, LuceneQueryParser.FieldType.FLOAT);
	}

	public void addField(Document doc, String key, OtpErlangLong value) {
		NumericField field = new NumericField(key, Field.Store.YES, true);
		field.setLongValue(value.longValue());
		doc.add(field);
		this.queryParser.putField(key, LuceneQueryParser.FieldType.LONG);
	}

	public void addField(Document doc, String key, OtpErlangInt value)
			throws UnsupportedFieldTypeException {
		try {
			NumericField field = new NumericField(key, Field.Store.YES, true);
			field.setIntValue(value.intValue());
			doc.add(field);
			this.queryParser.putField(key, LuceneQueryParser.FieldType.INT);
		} catch (OtpErlangRangeException e) {
			throw new UnsupportedFieldTypeException(value.getClass());
		}
	}

	public void addField(Document doc, String key, OtpErlangList value)
			throws UnsupportedFieldTypeException {
		if (value.arity() == 0) {
			doc.add(new Field(key, "", Field.Store.YES, Field.Index.ANALYZED));
			this.queryParser.putField(key, LuceneQueryParser.FieldType.STRING);
		} else {
			throw new UnsupportedFieldTypeException(value.getClass());
		}
	}

	public void addField(Document doc, String key, OtpErlangAtom value) {
		doc.add(new Field(key, value.atomValue(), Field.Store.YES,
				Field.Index.ANALYZED));
		this.queryParser.putField(key, LuceneQueryParser.FieldType.ATOM);
	}

	public void addField(Document doc, String key, OtpErlangString value) {
		doc.add(new Field(key, value.stringValue(), Field.Store.YES,
				Field.Index.ANALYZED));
		this.queryParser.putField(key, LuceneQueryParser.FieldType.STRING);
	}

}
