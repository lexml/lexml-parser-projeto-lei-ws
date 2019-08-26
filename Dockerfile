FROM tomcat:8.0-jre8-slim as build-base
RUN mkdir /opt/lexml && \
    apt-get update && \
    apt-get -y install git 

FROM tomcat:8.0-jre8-slim as runtime-base    
RUN apt-get update && \
    apt-get install -y abiword && \
    rm -fRv \
      /usr/local/tomcat/webapps/docs \
      /usr/local/tomcat/webapps/examples \
      /usr/local/tomcat/webapps/manager \
      /usr/local/tomcat/webapps/host-manager \
      /usr/local/tomcat/webapps/ROOT \
      /usr/local/tomcat/webapps/ROOT \
      /usr/lib/x86_64-linux-gnu/libLLVM* \
      /usr/lib/x86_64-linux-gnu/dri \
      /usr/share/icons && \
    sed -i -e '2iexport LANG=C.UTF-8 LC_ALL=C.UTF_8' /usr/local/tomcat/bin/catalina.sh

FROM lexmlbr/lexml-linker:1.2 as linker-base

FROM build-base as maven-base
RUN apt-get update && \
    apt-get -y install maven

FROM maven-base as build-step-2
WORKDIR /opt/lexml
RUN mkdir -p /root/.m2
COPY m2-settings.xml /root/.m2/settings.xml
ARG MAVEN_PROFILES=
COPY pom.xml .
RUN sed -e '0,/<version>/s/<version>\(.*\)<\/version>/<version>0.0.0<\/version>/' -i pom.xml
RUN mvn -P "$MAVEN_PROFILES" de.qaware.maven:go-offline-maven-plugin:resolve-dependencies && \
    rm pom.xml

FROM build-step-2 as build-step-3
RUN mkdir /opt/lexml/lexml-parser-projeto-lei-ws
WORKDIR /opt/lexml/lexml-parser-projeto-lei-ws
ARG MAVEN_PROFILES=
COPY pom.xml .
COPY src ./src
RUN find . -type f -print
RUN mvn -P "$MAVEN_PROFILES" clean package

FROM runtime-base
ARG uid
ARG gid
RUN mkdir -p /areastorage/parser/mensagemUsuario && \
    mkdir -p /areastorage/parser/results && \
    mkdir -p /areastorage/lexml-static && \
    groupadd -g $gid -r tomcat && \
    useradd -u $uid -r -g tomcat -d /usr/local/tomcat tomcat && \
    chown -R tomcat. /usr/local/tomcat && \
    chown -R tomcat. /areastorage
VOLUME /areastorage/parser
COPY --from=linker-base /usr/bin/simplelinker /usr/local/bin
USER tomcat:tomcat
WORKDIR /usr/local/tomcat
COPY --from=build-step-3 /opt/lexml/lexml-parser-projeto-lei-ws/target/lexml-parser.war ./webapps
COPY --from=build-step-3 /opt/lexml/lexml-parser-projeto-lei-ws/src/main/resources/lexml-static/ /areastorage/lexml-static
