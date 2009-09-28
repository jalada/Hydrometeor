all: compile

compile:
	mkdir -p ebin
	erl -make
	cp src/hydrometeor.app ebin/
	erl -pa ebin -s systools make_script hm_rel-1 -s init stop

clean:
	rm -rf ./ebin/*.*
