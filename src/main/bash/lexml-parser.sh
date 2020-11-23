#!/bin/bash

CLASSPATH=`find /lexml-parser-pl -name \*.jar -print0 | tr '\000' ':'`
export CLASSPATH
export LANG=C.UTF-8
export LC_ALL=C.UTF_8
java br.gov.lexml.parser.pl.ws.Main "$@"