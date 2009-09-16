all: compile

compile:
	mkdir -p ebin
	erl -make
	cp src/hydrometeor.app ebin/

clean:
	rm -rf ./ebin/*.*
