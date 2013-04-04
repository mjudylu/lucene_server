ERLANG := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -s wpool -setcookie tigertext_lucene ${ERL_ARGS}

all: clean
	rebar get-deps && rebar --verbose compile

erl:
	rebar skip_deps=true --verbose compile

clean:
	rebar clean

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools \
				    compiler --output_plt ~/.lucene_server_plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.lucene_server_plt -Werror_handling ebin

xref: all
	rebar skip_deps=true xref

shell: erl
	if [ -n "${NODE}" ]; then ${ERLANG} -name ${NODE}@`hostname` -boot start_sasl; \
	else ${ERLANG} -name lucene_server@`hostname` -boot start_sasl; \
	fi

run: erl
	if [ -n "${NODE}" ]; then ${ERLANG} -name ${NODE}@`hostname` -boot start_sasl -s lucene_server; \
	else ${ERLANG} -name lucene_server@`hostname` -boot start_sasl -s lucene_server; \
	fi

test: erl
	mkdir -p log/ct
	rebar skip_deps=true ct -vvv
	open log/ct/index.html

doc: erl
	rebar skip_deps=true doc
	javadoc -overview doc/overview-summary.html \
			-classpath ./bin:/usr/local/lib/erlang/lib/jinterface-1.5.6/priv/OtpErlang.jar:./priv/commons-pool-1.4.jar:./priv/jedis-2.1.0.jar:./priv/geonames-source-1.1.6.jar:priv/lucene-core-3.6.2.jar:priv/lucene-spatial-3.6.2.jar:priv/lucene-queryparser-3.6.2.jar \
			-verbose -d doc/java -use -version -author `find java_src -name *.java`