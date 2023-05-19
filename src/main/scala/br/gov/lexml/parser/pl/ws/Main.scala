package br.gov.lexml.parser.pl.ws

import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.routing.SmallestMailboxPool
import br.gov.lexml.parser.pl.ws.resources.ParserServiceActor
import grizzled.slf4j.{Logger, Logging}
import io.prometheus.client.exporter.MetricsServlet
import io.prometheus.client.filter.MetricsFilter
import io.prometheus.client.hotspot.DefaultExports
import io.prometheus.client.jetty.JettyStatisticsCollector
import org.apache.logging.log4j.core.config.Configurator
import org.apache.logging.log4j.core.config.ConfigurationSource
import org.eclipse.jetty.server.handler.StatisticsHandler
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.{FilterHolder, ServletContextHandler, ServletHolder}
import org.eclipse.jetty.util.component.LifeCycle.start
import org.glassfish.jersey.media.multipart.MultiPartFeature
import org.glassfish.jersey.servlet.ServletContainer




object Main {

  def main(args: Array[String]): Unit = {
    val environment =
      sys.env.get("LEXML_PARSER_APP_ENV")
        .filter(Set("desenvolvimento", "homologacao", "producao", "local-docker"))
        .getOrElse("local")
    System.err.println(s"lexml-parser-projeto-lei-ws iniciando em ambiente '$environment'")
    sys.props.put("lexml.parser.ws.env",environment)
    System.err.println("Loading configuration")
    LexmlWsConfig.init(environment)
    System.err.println("Initializing logging")
    initLogging(environment)
    val main = new Main(environment)
    main.run(args)
    start(environment)
  }

  def initLogging(environment : String) : Unit = {
    sys.props.put("log4j2.contextSelector",
      "org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")
    val log4jConfigPath = s"config/$environment/log4j2.xml"
    val source = new ConfigurationSource(
      Thread.currentThread().getContextClassLoader.
        getResourceAsStream(log4jConfigPath))
    System.err.println(s"Initializing Log4j2 using resource in '$log4jConfigPath'")
    Configurator.initialize(null, source)

  }

  private var _parserServiceRouter : Option[ActorRef] = None
  def setParserServiceRouter(r : ActorRef) = {
    _parserServiceRouter = Some(r)
  }
  def parserServiceRouter : ActorRef =
    _parserServiceRouter.getOrElse(
      throw new RuntimeException("parserServiceRouter not initialized yet")
    )
}

class Main(environment : String) extends Logging {

  def run(args : Array[String]) : Unit = {
    info("Initializing Hotspot metrics collectors")
    DefaultExports.initialize()

    info("Initializing the Cache")
    setupCache()

    info("Creating actor system")
    val (system,parserServiceRouter) = makeActorSystem()

    Main.setParserServiceRouter(parserServiceRouter)

    try {
      info("initializing MIME registry")
      MimeExtensionRegistry.init()

      info("Initializing AppServer")
      val server = makeWebServer()
      server.start()
      try {
        info("Joining AppServer")
        server.join()
      } finally {
        info("Destroying AppServer")
        server.destroy()
      }
    } finally {
      info("shutting down the actor system")
      system.terminate()
    }
    info("Exiting application")
  }


  def makeWebServer() : Server = {
    debug("creating jetty server")
    val server = new Server()

    val port = LexmlWsConfig.config.getInt("jetty.port") //8080
    debug(s"configuring server to listen on port $port")
    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.setConnectors(Array(connector))

    val ctxPath = LexmlWsConfig.appConfig.getString("base-context") // "/lexml-parser/"
    debug(s"creating context handler (ctx) for path $ctxPath")

    val ctx = new ServletContextHandler(
      null,ctxPath,
      ServletContextHandler.SESSIONS|ServletContextHandler.NO_SECURITY)
    //ctx.setContextPath(ctxPath)

    debug("adding metrics filter, with path = /parse/*")
    val mf = new MetricsFilter("servlet","Servlet metrics", 3,
      Array[Double](0.005,0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2.5,5,7.5,10))
    val fh = new FilterHolder(mf)
    ctx.addFilter(fh,"/parse/*",null)


    debug("creating servlet holder (serHol) with path spec = /*")
    val serHol: ServletHolder = ctx.addServlet(classOf[ServletContainer], "/*")
    serHol.setInitOrder(1)
    serHol.setInitParameter("jersey.config.server.provider.classnames",
      classOf[MultiPartFeature].getName)
    serHol.setInitParameter("jersey.config.server.provider.packages",
      this.getClass.getPackage.getName)

    debug("adding metrics export servlet to context")
    ctx.addServlet(classOf[MetricsServlet],"/sfstatus/metrics")

    debug("creating statics handler for jetty server metrics")
    val statisticsHandler = new StatisticsHandler()
    statisticsHandler.setServer(server)
    statisticsHandler.setHandler(ctx)

    new JettyStatisticsCollector(statisticsHandler).register()

    server.setHandler(statisticsHandler)
    debug("server configured")
    server
  }

  def setupCache() : Unit = {
    DataCache.init()
  }

  def makeActorSystem(): (ActorSystem,ActorRef) = {
    val system: ActorSystem = ActorSystem("lexml-parser-system")

    import akka.actor.SupervisorStrategy._

    val parserServiceSupervisionStrategy: OneForOneStrategy = OneForOneStrategy() {
      case _ : Exception => Restart
    }

    val parserServiceRouter: ActorRef =
      system.actorOf(Props[ParserServiceActor]().withRouter(SmallestMailboxPool(8,
        supervisorStrategy = parserServiceSupervisionStrategy)))

    (system,parserServiceRouter)
  }

}
