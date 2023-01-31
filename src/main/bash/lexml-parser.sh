#!/bin/bash

#get_ips() {
#	http -j GET 'http://rancher-metadata/2015-12-19/self/stack' \
#		| jq -r '.services[].containers[] | select(.service_name | test("^lexml-parser$")) | select (.state != "stopped") | .ips[0]' \
#		| grep -E -o "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
#}

CLASSPATH=`find /lexml-parser-pl -name \*.jar -print0 | tr '\000' ':'`
export CLASSPATH
export LANG=C.UTF-8
export LC_ALL=C.UTF_8

#PARSER_CLUSTER_SIZE="${PARSER_CLUSTER_SIZE:-2}"

#echo "Pause de 10s antes de procurar o cluster"
#sleep 10
#echo "Esperando IPs do cluster ...." > /dev/stderr
#FOUND=0
#while true ; do
#	ips=( $(get_ips) )
#	FOUND="${#ips[@]}"
#	if (( $FOUND != $PARSER_CLUSTER_SIZE )) ; then
#		echo "... cluster ainda não subiu totalmente. $FOUND/$PARSER_CLUSTER_SIZE" > /dev/stderr
#		sleep 1
#	else
#		echo "IPs localizados: ${ips[@]}"
#		break
#	fi
#done

#export cluster_ips="${ips[@]}"

#echo "cluster_ips=${cluster_ips}"

#java br.gov.lexml.parser.pl.ws.Main "-Dcluster_ips=${ips[@]}" "$@"
java br.gov.lexml.parser.pl.ws.Main "$@"
