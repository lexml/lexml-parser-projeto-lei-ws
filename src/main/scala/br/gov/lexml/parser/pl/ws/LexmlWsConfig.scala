package br.gov.lexml.parser.pl.ws

import com.typesafe.config.{Config, ConfigFactory}

import java.io.File

object LexmlWsConfig {
  private var environment : Option[String] = None
  private var _config : Option[Config] = None
  def env = environment.getOrElse(throw new RuntimeException("Environment not set yet"))
  def init(env : String) = this.synchronized {
    environment match {
      case None => environment = Some(env)
      case Some(x) => throw new RuntimeException(s"LexmlWsConfig alread initialized for env $x")
    }
    _config = Some {
      val baseConfig = ConfigFactory.load("config/application.conf")
      ConfigFactory.load(s"config/$env/application.conf")
        .withFallback(baseConfig)
    }
  }

  def appConfig : Config = config.getConfig("lexml.parser.ws")

  def config : Config = _config.getOrElse(throw new RuntimeException("Config not loaded yet"))

  lazy val parserResultDir = new File(appConfig.getString("cache.directory"))

}