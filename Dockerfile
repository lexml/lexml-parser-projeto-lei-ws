FROM tomcat:8.0-jre8-slim
ARG uid
ARG gid
RUN groupadd -g $gid -r tomcat && \
    useradd -u $uid -r -g tomcat -d /usr/local/tomcat tomcat && \
    mkdir -p /areastorage/parser/mensagemUsuario && \
    mkdir -p /areastorage/parser/results && \
    mkdir -p /areastorage/lexml-static && \
    mkdir -p /opt/lexml
VOLUME /areastorage/parser
RUN apt-get update && \
    apt-get -y install abiword maven wget git 
RUN wget -qO- https://get.haskellstack.org/ | sh
WORKDIR /opt/lexml
RUN git clone https://github.com/lexml/lexml-linker.git && \
    cd lexml-linker && \
    stack install --local-bin-path /usr/bin alex happy && \
    stack install --local-bin-path /usr/bin && \
    cp /usr/bin/simplelinker /usr/local/bin && \
    rm -fr /root/.stack
WORKDIR /usr/local/tomcat
RUN chown -R tomcat. /usr/local/tomcat && \
    chown -R tomcat. /areastorage && \
    rm -fr /opt/lexml
USER tomcat:tomcat
COPY target/lexml-parser.war webapps
COPY src/main/resources/lexml-static /areastorage/lexml-static
