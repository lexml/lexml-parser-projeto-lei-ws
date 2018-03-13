package br.gov.lexml.parser.pl.ws

import com.typesafe.config.{Config, ConfigFactory}

object LexmlWsConfig {
  val config: Config = ConfigFactory.load().getConfig("lexml.parser.ws")
}