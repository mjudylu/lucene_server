package com.tigertext.lucene.ext;

import java.util.logging.Logger;

import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.ext.ExtensionQuery;
import org.apache.lucene.queryParser.ext.ParserExtension;
import org.apache.lucene.search.ConstantScoreQuery;
import org.apache.lucene.search.Filter;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.function.CustomScoreQuery;
import org.apache.lucene.search.function.ValueSource;
import org.apache.lucene.search.function.ValueSourceQuery;

import com.tigertext.lucene.DocumentTranslator;

/**
 * Extension to run ".erlang" queries. These queries should be in the form
 * Field.erlang:Mod:Fun:Args where -spec Mod:Fun(Args, string()) -> false |
 * float(). If Mod:Fun(Args..., Value) returns false, the document doesn't match
 * Otherwise the result is the score. Args will be parsed on the Erlang node
 * using {ok, A, _} = erl_scan:string(Args++"."). {ok, B} =
 * erl_parse:parse_exprs(A). {value, Arguments, _} = erl_eval:exprs(B, []). So
 * the final call will be erlang:apply(Mod, Fun, Arguments ++ [Value]).
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class ErlangParserExtension extends ParserExtension {
	private static final Logger	jlog	= Logger.getLogger(ErlangParserExtension.class
												.getName());
	private DocumentTranslator	translator;

	/**
	 * Creates a new parser extension
	 * 
	 * @param translator
	 *            Used to determine the type of fields
	 */
	public ErlangParserExtension(DocumentTranslator translator) {
		super();
		this.translator = translator;
	}

	@Override
	public Query parse(ExtensionQuery extQuery) throws ParseException {
		String key = extQuery.getField();
		String[] modFun = extQuery.getRawQueryString().split(":");
		if (modFun.length < 2) {
			throw new ParseException(
					"erlang queries expect values in <mod>:<fun> or <mod>:<fun>:<args> format");
		} else {
			String mod = modFun[0], fun = modFun[1], args;
			if (modFun.length == 2) {
				args = "[]";
			} else if (modFun.length == 3) {
				args = modFun[2];
			} else {
				args = "";
				String colon = "";
				for (int i = 2; i < modFun.length; i++) {
					args += colon + modFun[i];
					colon = ":";
				}
			}

			jlog.fine("erlang query using " + key + " and " + mod + ":" + fun
					+ "(" + args + "...");

			Filter filter = new ErlangFilter(mod, fun, args, key,
					this.translator.getFieldType(key));

			ValueSource valSrc = new ErlangValueSource((ErlangFilter) filter);

			return new CustomScoreQuery(new ConstantScoreQuery(filter),
					new ValueSourceQuery(valSrc));
		}
	}

}
