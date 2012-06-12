package com.tigertext.lucene;

public class UnsupportedFieldTypeException extends Exception {

	public UnsupportedFieldTypeException(String type) {
		super("Unsupported field type: " + type);
	}

	private static final long serialVersionUID = 8966657853257773634L;

}
