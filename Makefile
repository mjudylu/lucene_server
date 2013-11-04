ERLANG := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -s wpool -setcookie tigertext_lucene ${ERL_ARGS}
CLASSPATH := ./bin:"./priv/*"

all: clean
	./rebar get-deps && ./rebar --verbose compile

erl:
	./rebar skip_deps=true --verbose compile

clean:
	./rebar clean

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools \
				    compiler --output_plt ~/.lucene_server_plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.lucene_server_plt -Werror_handling ebin

xref: all
	./rebar skip_deps=true xref

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
	mkdir -p log/java
	java -classpath ${CLASSPATH} net.sourceforge.cobertura.instrument.Main bin
	jar cf priv/lucene-server.jar -C bin .
	./rebar skip_deps=true ct -vvv
	java -classpath ${CLASSPATH} net.sourceforge.cobertura.reporting.Main --destination log/java java_src
	open log/ct/index.html
	open log/java/index.html

doc: erl
	./rebar skip_deps=true doc
	javadoc -overview doc/overview-summary.html \
			-classpath ${CLASSPATH} \
			-verbose -d doc/java -use -version -author `find java_src -name *.java`
