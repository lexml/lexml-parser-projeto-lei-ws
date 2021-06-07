#!/bin/bash

CLASSPATH=`find /lexml-parser-pl -name \*.jar -print0 | tr '\000' ':'`
export CLASSPATH
export LANG=C.UTF-8
export LC_ALL=C.UTF_8
java \ 
	--add-modules java.se \
	--add-exports java.base/jdk.internal.ref=ALL-UNNAMED \
	--add-opens java.base/java.lang=ALL-UNNAMED \
        --add-opens java.base/java.nio=ALL-UNNAMED \
        --add-opens java.base/sun.nio.ch=ALL-UNNAMED \
        --add-opens java.management/sun.management=ALL-UNNAMED \
        --add-opens jdk.management/com.sun.management.internal=ALL-UNNAMED \
	br.gov.lexml.parser.pl.ws.Main \
	"$@"
