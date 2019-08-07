#!/bin/bash

VERSION="${1:-latest}"
IMAGE_NAME_PREFIX="${IMAGE_NAME_PREFIX:-lexmlbr/parser-projeto-lei-ws}"
IMAGE_NAME="${IMAGE_NAME_PREFIX}:${VERSION}"
if [ "$REGISTRY" == "" ] ; then
  TAG="$IMAGE_NAME"
else
  TAG="$REGISTRY/$IMAGE_NAME"
fi

echo "Building image tagged as $TAG"

function getExtraParameters {
  if [ ! -z "$http_proxy" ]; then
    PROXY_BASE=$(echo $http_proxy | cut -d/ -f3)
    PROXY_HOST=$(ip addr list docker0 | grep "inet " | cut -d' ' -f6 | cut -d/ -f1)
    PROXY_PORT=$(echo $PROXY_BASE | cut -d: -f2) 
    PROXY="http://"$(ip addr list docker0 |grep "inet " |cut -d' ' -f6|cut -d/ -f1)":3128"
    echo "--build-arg http_proxy=$PROXY --build-arg https_proxy=$PROXY --build-arg http_host=$PROXY_HOST --build-arg http_port=$PROXY_PORT"
  else
    echo ""
  fi
}
EXTRA_PARAMS="$EXTRA_PARAMS $(getExtraParameters)"
echo "Extra parameters: $EXTRA_PARAMS"
if [ -f "m2-settings.xml" ] ; then
  echo "Custom Maven settings found"
elif [ $EXTRA_PARAMS =~ "MAVEN_PROFILES=[^ ]*senado" ] ; then  
  echo "Generating m2-settings for build at Senado"
  cat > m2-settings.xml <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd">
  <mirrors>
    <mirror>
      <id>central-proxy</id>
      <name>Local proxy of central repo</name>
      <url>http://www20.senado.gov.br/artifactory/repo</url>
      <mirrorOf>central</mirrorOf>
    </mirror>
  </mirrors>
</settings>  
EOF
else
  echo "Generating general m2-settings"
  cat > m2-settings.xml <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd">
</settings>  
EOF
fi

VER=`git log | head -n 1 | sed -e 's/commit *//g'`
echo "$VER" > src/main/resources/lexml-static/commit-id
sed -i -e "s/VERSAO_PARSER/$VER/g" src/main/resources/lexml-static/simulador/simulador.html  

docker build ${EXTRA_PARAMS} --build-arg uid=2000 --build-arg gid=2000 --build-arg "version=${VERSION}" . -t "$TAG"
