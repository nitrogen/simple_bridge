all: compile

compile:
	mkdir -p ebin
	erl \
		-pa ./ebin \
		-make
	
clean:
	rm -rf ./ebin/*.*

test:
	git submodule update --init lib/etap
	make -C lib/etap