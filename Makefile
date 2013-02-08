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
	echo "Killing: " `ps aux | grep "LuceneNode" | grep -v "grep" | awk '{print $$2}'`
	kill -9 `ps aux | grep "LuceneNode" | grep -v "grep" | awk '{print $$2}'`
	open log/ct/index.html

doc: erl
	rebar skip_deps=true doc