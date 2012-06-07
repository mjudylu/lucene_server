JAVA   := java -classpath ./bin:/usr/local/lib/erlang/lib/jinterface-1.5.6/priv/OtpErlang.jar:./priv/lucene-core-3.6.0.jar com.tigertext.lucene.LuceneNode
ERLANG := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -setcookie tigertext_lucene ${ERL_ARGS}

all:
	rebar get-deps && rebar compile

erl:
	rebar compile

clean: java_clean
	rebar clean

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools \
				    compiler --output_plt ~/.lucene_server_plt -pa deps/*/ebin apps/*/ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.lucene_server_plt -Werror_handling apps/*/ebin

xref: all
	rebar skip_deps=true xref

shell: erl
	${ERLANG} -boot start_sasl

run: erl
	if [ -n "${NODE}" ]; then ${ERLANG} -name ${NODE}@`hostname` -boot start_sasl -s lucene_server; \
	else ${ERLANG} -name lucene_server@`hostname` -boot start_sasl -s lucene_server; \
	fi

java_clean:
	rm -rf bin/*

java_build:
	mkdir -p bin
	javac -g -verbose -deprecation -sourcepath java_src -classpath ./bin:/usr/local/lib/erlang/lib/jinterface-1.5.6/priv/OtpErlang.jar:./priv/lucene-core-3.6.0.jar -d bin `find java_src -name *.java`

jar: java_build
	rm -f priv/lucene_server.jar
	cd bin && jar cf ../priv/lucene_server.jar *

java_run: java_build
	epmd -daemon && \
	if [ -n "${NODE}" ]; then ${JAVA} ${NODE}@`hostname` tigertext_lucene; \
	else ${JAVA} lucene@`hostname` tigertext_lucene; \
	fi

test: erl
	mkdir -p log/ct
	rebar skip_deps=true ct
	open log/ct/index.html