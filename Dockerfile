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

FROM lexmlbr/lexml-linker:1.4.4 as linker-base

FROM build-base as maven-base
RUN apt-get update && \
    apt-get -y install maven

FROM maven-base as build-step-2
WORKDIR /opt/lexml
RUN mkdir -p /root/.m2
COPY m2-settings.xml /root/.m2/settings.xml
COPY pom.xml .
RUN sed -e '0,/<version>/s/<version>\(.*\)<\/version>/<version>0.0.0<\/version>/' -i pom.xml
RUN mvn de.qaware.maven:go-offline-maven-plugin:resolve-dependencies && \
    rm pom.xml

FROM build-step-2 as build-step-3
RUN mkdir /opt/lexml/lexml-parser-projeto-lei-ws
WORKDIR /opt/lexml/lexml-parser-projeto-lei-ws
COPY pom.xml .
COPY src ./src
RUN find . -type f -print
RUN mvn clean && \
    mvn dependency:analyze-report && \
    cp target/dependency-analysis.html src/main/resources/lexml-static/ && \
    mvn package && \
    rm target/lexml-parser.war

FROM runtime-base
ARG uid
ARG gid
RUN groupadd -g $gid -r tomcat && \
    useradd -u $uid -r -g tomcat -d /usr/local/tomcat tomcat && \
    chown -R tomcat. /usr/local/tomcat
COPY --from=linker-base /usr/bin/simplelinker /usr/bin/linkertool /usr/local/bin/
USER tomcat:tomcat
WORKDIR /usr/local/tomcat
COPY --from=build-step-3 /opt/lexml/lexml-parser-projeto-lei-ws/target/lexml-parser/ ./webapps/lexml-parser
COPY --from=build-step-3 /opt/lexml/lexml-parser-projeto-lei-ws/pom.xml ./webapps/lexml-parser/WEB-INF/classes/lexml-static/lexml-parser-projeto-lei.pom
