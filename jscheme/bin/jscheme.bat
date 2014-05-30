@echo off
rem you must have java and javac in your path

set H=%~dp0
set H=%H:\bin\=%
javaw -jar %H%/lib/jscheme.jar %*