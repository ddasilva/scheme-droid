
# This is the Makefile for Scheme Droid. It orchestrates the building of
# Scheme Droid and JScheme. To do this it calls other build systems.
#
# This is the defacto build tool. Do not attempt to manually call ant or
# jscheme/bin/make.sh unless you know exactly what you are doing.

all:
	make debug

clean:
	rm -rf bin/*
	find jscheme -name '*.class' | xargs rm -f

jscheme:
	cd jscheme && ./bin/make.sh
	cd ..
	cp jscheme/lib/jscheme.jar libs/

debug:
	ant debug

install: debug
	ant installd

test:
	${ANDROID_HOME}/platform-tools/adb shell am instrument -w \
		net.meltingwax.schemedroid.test/android.test.InstrumentationTestRunner

.PHONY: all clean jscheme debug install test
