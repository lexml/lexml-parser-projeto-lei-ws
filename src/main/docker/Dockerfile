FROM lexmlbr/lexml-linker:1.4.5 as linker-base

FROM adoptopenjdk:11.0.9.1_1-jre-hotspot as runtime-base
RUN apt-get update    && \
    apt-get -y --no-install-recommends install abiword \
        dnsutils iputils-ping iproute2 vim wget lsof && \
#    groupadd -r -g 200 parser && \
#    useradd -r -g parser -u 200 -d /lexml-parser-pl parser  && \
    apt-get clean && \
    rm /var/lib/apt/lists/*.lz4 && \
    rm -fR /usr/share/icons && \
    mkdir -p /lexml-parser-pl/logs
COPY --from=linker-base /usr/bin/simplelinker /usr/bin/linkertool /usr/local/bin/
COPY maven/ /lexml-parser-pl
#RUN mkdir -p /lexml-parser-pl/logs && \
#    chown -R parser:parser /lexml-parser-pl
#USER parser:parser
VOLUME /lexml-parser-pl/logs
ENTRYPOINT ["/bin/bash", "/lexml-parser-pl/bin/lexml-parser.sh"]
