@echo off
rem you must have java and javac in your path
rem for documentation run: bin\make.bat -help
set H=%~dp0
set H=%H:\bin\=%

javac -classpath %H%/src "%H%/src/jscheme/REPL.java"
javaw -classpath "%H%/src" jscheme.REPL build/make.scm -main commandMain %*
