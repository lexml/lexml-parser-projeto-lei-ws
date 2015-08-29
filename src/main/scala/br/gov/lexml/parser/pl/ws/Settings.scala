package br.gov.lexml.parser.pl.ws

import com.typesafe.config.ConfigFactory

object LexmlWsConfig {
  val config = ConfigFactory.load().getConfig("lexml.parser.ws")
}