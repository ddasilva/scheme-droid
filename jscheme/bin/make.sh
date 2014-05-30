#! /bin/sh
# you must have java and javac in your path
# for documentation run: bin/make -help  
javac -classpath src src/jscheme/REPL.java
java -classpath src jscheme.REPL src/build/make.scm -main commandMain "$@"
