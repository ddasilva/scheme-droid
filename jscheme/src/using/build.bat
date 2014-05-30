rem @echo off
rem you must have java and javac in your path

set H=%~dp0
set H=%H:\src\using=%
java -classpath "%H%/src;%H%/lib/jscheme.jar" jscheme.REPL %H%/src/using/build.scm -main commandMain %*