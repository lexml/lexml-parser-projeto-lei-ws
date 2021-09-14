#!/bin/bash

get_cluster_ips() {
	http -j GET 'http://rancher-metadata/2015-12-19/self/stack' \
		| jq -r '.services[].containers[] | select(.service_name | test("^parser[0-9]+$")) | .ips[0]' \
		| grep -E -o "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)" 
}

CLASSPATH=`find /lexml-parser-pl -name \*.jar -print0 | tr '\000' ':'`
export CLASSPATH
export LANG=C.UTF-8
export LC_ALL=C.UTF_8

PARSER_CLUSTER_SIZE="${PARSER_CLUSTER_SIZE:-2}"

echo "Esperando IPs do cluster ...." > /dev/stderr
FOUND=0
while true ; do
	cluster_ips=( $(get_cluster_ips) )
	FOUND="${#cluster_ips[@]}"
	if (( $FOUND < $PARSER_CLUSTER_SIZE )) ; then
		echo "... cluster ainda não subiu totalmente. $FOUND/$PARSER_CLUSTER_SIZE" > /dev/stderr
		sleep 1
	else
		echo "IPs localizados: ${cluster_ips[@]}"
		break
	fi
done

export cluster_ips

java br.gov.lexml.parser.pl.ws.Main "-Dcluster_ips=${cluster_ips[@]}" "$@"
