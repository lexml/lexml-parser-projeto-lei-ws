FROM lexmlbr/lexml-linker:1.4.10 as linker-base

FROM eclipse-temurin:8u432-b06-jdk as runtime-base
RUN apt-get update    && \
    apt-get upgrade -y && \
    apt-get -y --no-install-recommends install abiword \
        dnsutils iputils-ping iproute2 vim wget lsof httpie \
	jq  netcat-traditional && \
#    groupadd -r -g 200 parser && \
#    useradd -r -g parser -u 200 -d /lexml-parser-pl parser  && \
    apt-get clean && \
    rm /var/lib/apt/lists/*.lz4 && \
    rm -fR /usr/share/icons && \
    mkdir -p /lexml-parser-pl/logs
#RUN (echo "#!/usr/bin/env sh" && curl -L https://github.com/com-lihaoyi/Ammonite/releases/download/2.4.0/2.13-2.4.0) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm
COPY --from=linker-base /usr/bin/simplelinker /usr/bin/linkertool /usr/local/bin/
COPY maven/ /lexml-parser-pl
#RUN mkdir -p /lexml-parser-pl/logs && \
#    chown -R parser:parser /lexml-parser-pl
#USER parser:parser
VOLUME /lexml-parser-pl/logs
ENTRYPOINT ["/bin/bash", "/lexml-parser-pl/bin/lexml-parser.sh"]
