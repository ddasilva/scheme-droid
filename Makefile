
# This is the Makefile for Scheme Droid. It orchestrates the building of
# Scheme Droid and JScheme. To do this it calls other build systems.
#
# This is the defacto build tool. Do not attempt to manually call ant or
# jscheme/bin/make.sh unless you know exactly what you are doing.

all:
	make debug

clean:
	rm -rf bin/*

jscheme:
	cd jscheme && ./bin/make.sh
	cd ..
	cp jscheme/lib/jscheme.jar libs/

debug: jscheme
	ant debug

install: debug
	ant installd

.PHONY: all clean jscheme debug install
