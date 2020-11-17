package br.gov.lexml.parser.pl.ws.resources

import java.io.{ByteArrayOutputStream, File, InputStream}
import java.net.URI
import java.security.SecureRandom

import javax.annotation.Resource
import javax.servlet.http.HttpServletRequest
import javax.ws.rs._
import javax.ws.rs.core.{Context, MediaType, Response, UriInfo}
import javax.xml.ws.WebServiceContext
import javax.xml.ws.handler.MessageContext
import akka.actor.Actor
import br.gov.lexml.parser.pl.ws.{Initializer, ServiceParams}
import br.gov.lexml.parser.pl.ws.data.ParserRequisicao
import br.gov.lexml.parser.pl.ws.resources.proc.{RequestContext, RequestProcessor}
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import com.sun.jersey.core.header.FormDataContentDisposition
import com.sun.jersey.multipart.FormDataParam
import eu.medsea.mimeutil.MimeUtil
import grizzled.slf4j.Logging
import org.apache.commons.codec.binary.Base32
import org.apache.commons.io.{FileUtils, IOUtils}

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.xml.XML
import java.nio.charset.Charset
import java.time.ZonedDateTime
import java.util.regex.Pattern

import br.gov.lexml.parser.pl.ws.LexmlWsConfig
import javax.ws.rs.core.CacheControl
import io.prometheus.client.Counter
import io.prometheus.client.Gauge



object ScalaParserService {
  val parserJobsCounter: Counter = Counter.build().name("lexml_parser_job_count").help("Total de trabalhos do Parser").register()
  val parserJobsInProgress: Gauge = Gauge.build().name("lexml_parser_jobs_inprogress").help("Trabalhos do Parser em execução").register()
}

@Path("/parse")
class ScalaParserService extends Logging {
  import ScalaParserService._
  
  @Resource
  var context : WebServiceContext = _

  @Context
  var uriInfo: UriInfo = _

  private def buildURIFromRelativePath(path: String*) = uriInfo match {
    case null => new URI("http://localhost:8180/parser" + path.mkString(";", ";", ""))
    case _ =>
      val builder = uriInfo.getBaseUriBuilder
      //builder.scheme("https")
      for (c <- path) builder.path(c)
      builder.build()
  }

  @POST
  @Path("parseSenado")
  @Consumes(Array(MediaType.MULTIPART_FORM_DATA))
  def parseSenado(@FormDataParam("requisicao") requisicaoText: String,
    @FormDataParam("fonte") fonteStream: InputStream,
    @FormDataParam("fonte") fonteDetail: FormDataContentDisposition): Response = {    
    if(fonteStream == null) {
      throw new RuntimeException("Erro: fonteStream == null")
    }
    doParseSenado(requisicaoText, Some(IOUtils.toByteArray(fonteStream)),Some(fonteDetail.getFileName))
  }

  @POST
  @Path("parseSenado")
  @Consumes(Array("text/xml"))
  @Produces(Array("text/xml"))
  def parseSenado(requisicaoText: String): Response = {
    doParseSenado(requisicaoText)
  }

  @GET
  @Path("result/{id}")
  @Produces(Array("text/xml"))
  def readResultGet(@PathParam("id") id: String): Response = {
    logger.info("readResultGet: id = " + id)    
    Response.temporaryRedirect(uriInfo.getRequestUriBuilder.path("resultado.xml").build()).build
  }

  
  @Path("result/{id}/{filename}")
  @GET
  def readResult(@PathParam("id") id: String, @PathParam("filename") filename: String): Response =
    touchId(id) { doReadResult(id, filename) }

  
  
  
  @GET
  @Path("result/{id}/{dir}/{filename}")
  def readResult(@PathParam("id") id: String, @PathParam("filename") filename: String,
    @PathParam("dir") dir: String): Response =
    touchId(id) { doReadResult(id, dir, filename) }

  private def touchSession() {
    if (context != null) {
      val mc = context.getMessageContext
      val session = mc.get(MessageContext.SERVLET_REQUEST).asInstanceOf[HttpServletRequest].getSession()
      logger.info("Session: " + session.getId)
    }
  }
  
  private def doParseSenado(requisicaoText: String, fonte: Option[Array[Byte]] = None, fileName : Option[String] = None): Response = {
    logger.info(s"doParseSenado: starting. requisicaoText.length=${requisicaoText.length()}, fonte.size=${fonte.map(_.length).toString}, fileName=${fileName.toString}")
    val boot = Initializer.boot.get
    touchSession()
    try{
      val requisicao = ParserRequisicao.fromXML(XML.loadString(requisicaoText))

      var uniqueId: String = null
      var resultPath: File = null
      do {
        uniqueId = newExecutionId()
        resultPath = new File(ServiceParams.params.parseResultDirectory, uniqueId)
      } while (resultPath.exists())

      def resultURI(comps: String*): URI = {
        buildURIFromRelativePath("parse" :: "result" :: uniqueId :: comps.toList : _*)
      }

      FileUtils.forceMkdir(resultPath)
      val waitFile = new File(resultPath, "wait")
      FileUtils.touch(waitFile)
      val dataHoraProcessamento = ZonedDateTime.now()
      logger.info("sending message to actor with uniqueId=%s, requisicao=%s, resultPath=%s, waitFile=%s, dataHoraProcessamento=%s".format(uniqueId, requisicao, resultPath, waitFile, dataHoraProcessamento))
      parserJobsCounter.inc()
      parserJobsInProgress.inc()
      boot.parserServiceRouter ! new RequestProcessor(RequestContext(resultURI, uniqueId, requisicao, resultPath, waitFile, dataHoraProcessamento, fonte, fileName))
      val uri = resultURI()
      logger.info("result for " + uniqueId + ", at  " + uri)
      Response.created(uri).entity(<Location>{ uri.toURL.toExternalForm }</Location>.toString).build()
    } catch {
      case ex: Exception =>
        logger.error("Erro durante a execução: " + ex.getMessage + ", input: " + requisicaoText, ex)
        Response.status(Response.Status.BAD_REQUEST).build()

    } finally {
      logger.info(s"doParseSenado: ending. requisicaoText.length=${requisicaoText.length()}, fonte.size=${fonte.map(_.length).toString}, fileName=${fileName.toString}")
    }
  }

  private lazy val random = new SecureRandom()

  private def newExecutionId(): String = {
    val a = new Array[Byte](5)
    synchronized {
      random.nextBytes(a)
    }
    new Base32().encodeAsString(a)
  }

  val notFoundBuilder: Response.ResponseBuilder = Response.status(Response.Status.NOT_FOUND).header("Cache-control", "no-cache") //"s-maxage=1")
  val notFound: Response = notFoundBuilder.build()

  private lazy val resultado2XhtmlData = {
    val is = getClass.getClassLoader.getResourceAsStream("resultado2xhtml.xsl")
    val os = new ByteArrayOutputStream()
    IOUtils.copy(is, os)
    os.toByteArray
  }

  private def getResultado2XhtmlData: Array[Byte] = {
    resultado2XhtmlData
  }

  private def doReadResult(id: String, pathComps: String*): Response = {    
    val res = pathComps match {
      case Seq("resultado2xhtml.xsl") =>
        Response.ok(getResultado2XhtmlData, "application/xslt+xml")

      case _ => doReadResult2(id, pathComps: _*).map(r => Response.ok(r._1, r._2))
        .getOrElse(notFoundBuilder)
    }
    res.header("Cache-Control","private").build()
  }

  private def doReadResult2(id: String, pathComps: String*): Option[(File, String)] = {    
    logger.info("doReadResult2: id = " + id + ", pathComps = " + pathComps)
    val resultPath = new File(ServiceParams.params.parseResultDirectory, id)
    val waitFile = new File(resultPath, "wait")
    val pathComps2 = if (pathComps.isEmpty) { List("resultado.xml") } else pathComps
    val reqFile = pathComps2.foldLeft(resultPath)(new File(_, _))
    if (reqFile.getPath != waitFile.getPath && waitFile.exists()) {
      None
    } else if (!reqFile.exists()) {
      None
    } else {
      val mimeFile = new File(reqFile.getParentFile, reqFile.getName + ".mime")
      val mimeType = FileUtils.readFileToString(mimeFile, Charset.forName("utf-8"))
      logger.info("responding to get with reqFile = " + reqFile + ", and mimeType = " + mimeType)
      Some((reqFile, mimeType))
    }
  }
  val idRegex: Regex = "^[A-Z0-9]+$"r
  def touchId(id: String)(rest: => Response): Response = {
    touchSession()
    if (idRegex.findFirstIn(id).isDefined) {
      ServiceParams.params.parseResultDirectory.list()
      val resultDir = new File(ServiceParams.params.parseResultDirectory, id)
      if (resultDir.exists && resultDir.isDirectory) {
        resultDir.listFiles()
        val waitFile = new File(resultDir, "wait")
        if (!waitFile.exists) {
          FileUtils.touch(resultDir)
          rest
        } else {
          notFound
        }
      } else { notFound }
    } else { notFound }
  }

  private def getMostSpecificMimeType(f: File, filterAccept: Boolean = true): Option[String] = {
    import scala.jdk.javaapi.CollectionConverters._
    def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
    val availableMimes = asScala(MimeUtil.getMimeTypes(f)).toList
    val accepted = if (filterAccept) { availableMimes.filter(accept) } else { availableMimes }
    if (accepted.isEmpty) {
      None
    } else {
      Some(MimeUtil.getMostSpecificMimeType(asJava(accepted)).toString)
    }
  }
  
  private def getMostSpecificMimeType(b : Array[Byte], filterAccept: Boolean): Option[String] = {
    import scala.jdk.javaapi.CollectionConverters._
    def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
    val availableMimes = asScala(MimeUtil.getMimeTypes(b)).toList
    val accepted = if (filterAccept) { availableMimes.filter(accept) } else { availableMimes }
    if (accepted.isEmpty) {
      None
    } else {
      Some(MimeUtil.getMostSpecificMimeType(asJava(accepted)).toString)
    }
  }
   
  @Path("static/{file: .*}")
  @GET
  def serveStatic(@PathParam("file") file: String): Response = {
    logger.debug("serveStatic: file = " + file)    
    val staticDir = ServiceParams.params.staticOverrideDirectory
    val f = new File(new File(staticDir, "lexml-static"),file).getCanonicalFile
    logger.debug("staticDir: " + staticDir)
    logger.debug("serving: " + f)
    logger.debug("f.exists = " + f.exists + ", f.isFile = " + f.isFile)
    val noFilterAccept = false
    if (f.getPath.startsWith(staticDir.getPath) && f.exists() && f.isFile) {

      val mt = getMostSpecificMimeType(f, noFilterAccept)
      logger.debug("serveStatic: found on static dir. f = " + f + ", mt = " + mt)
      Response.ok(f, mt.getOrElse("application/binary")).header("Cache-control", "public").build()
    } else {      
      Option(this.getClass.getResourceAsStream("lexml-static/" + file)) match {
        case None =>
          logger.info("serveStatic: not found.")
          notFound
        case Some(is) =>
          val d = IOUtils.toByteArray(is)
          val mt = getMostSpecificMimeType(d, noFilterAccept)
          logger.debug("serveStatic: found on classpath. mt = " + mt)
          Response.ok(d, mt.getOrElse("application/binary")).header("Cache-control", "public").build()

      }
    }
  }
  
  @POST
  @Path("renderDocx")    
  @Consumes(Array(MediaType.MULTIPART_FORM_DATA))
  @Produces(Array("application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
  def renderDocx(
    @FormDataParam("fonte") fonteStream: InputStream,
    @FormDataParam("fonte") fonteDetail: FormDataContentDisposition): Response = {    
    if(fonteStream == null) {
      throw new RuntimeException("Erro: fonteStream == null")
    }
    val source = IOUtils.toByteArray(fonteStream)    
    val fileName = fonteDetail.getFileName    
    val outputFileName = if(fileName.endsWith(".xml")) {
      fileName.substring(0,fileName.lastIndexOf(".xml")) + ".docx"
    } else { 
      fileName + ".docx"
    }    
    import br.gov.lexml.renderer.docx._    
    val cfg = LexmlToDocxConfig()
    val r = new LexmlToDocx(cfg)
    try {
      val res = r.convert(source)     
      Response
        .ok(res)
        .`type`("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        .header("Content-Disposition", s"""attachment; filename=$outputFileName""")
        .header("Cache-control","no-cache, no-store, no-transform, must-revalidate")
        .build()
    } catch {
      case ex : Exception =>        
        logger.error("erro na geração de DOCX: " + ex.getMessage,ex)        
        Response.serverError().build()
    }
  }
  
  
  private val linkerToolPath =
    LexmlWsConfig.config.getString("linkerTool")
        
  @POST
  @Path("linkerDecorator")
  //@Consumes(Array(MediaType.TEXT_PLAIN,MediaType.TEXT_HTML,MediaType.TEXT_XML))
  @Produces(Array(MediaType.TEXT_HTML))
  def linkerDecorator(
      @QueryParam("contexto") contextStr : String,
      @QueryParam("resolver") resolverStr : String,
      @Context request : HttpServletRequest,      
      content : Array[Byte]) : Array[Byte] = {    
    val context = Option(contextStr).map(_.trim()).filterNot(_.isEmpty)
    val somenteLinks = request.getParameterMap.containsKey("somenteLinks")
    val linksParaSiteLexml = request.getParameterMap.containsKey("linksParaSiteLexml")
    val resolver = Option(resolverStr).map(_.trim).filter(_.contains("URNLEXML"))
          .getOrElse("http://www.lexml.gov.br/urn/URNLEXML")
    import scala.sys.process._
    val tipo = 
      if (request.getContentType.startsWith(MediaType.TEXT_PLAIN)) { "--text" }
      else { "--hxml" }
    
    val saida = if (somenteLinks) { "--urns" } else { "--html" }
    val ctx = context.map(x => s"--contexto=$x").getOrElse("--contexto=federal")    
    val output = new java.io.ByteArrayOutputStream
    Process(Seq(linkerToolPath,tipo,saida,ctx,s"--enderecoresolver=$resolver"))
        .#<(new java.io.ByteArrayInputStream(content))
        .#>(output)
        .!(ProcessLogger(_ => ()))
    val out = output.toByteArray
    val outStr = new String(out,"utf-8")
    if(!linksParaSiteLexml && !somenteLinks) {
      val pat = resolver.replaceAll("URNLEXML","""(urn:lex:[^"]*)""")
      val linkerHrefPattern =         
        Pattern.compile(s"""<a href="$pat"[^>]*>(.*?)</a>""",
          Pattern.DOTALL)
      linkerHrefPattern.matcher(outStr).replaceAll("""<remissao xlink:href="$1">$2</remissao>""")
          .getBytes("utf-8")
    } else { outStr.getBytes("utf-8") }
  }
        
}

@Path("/sfstatus")
class ParserServiceHealth extends Logging {

  private val noCache = {
    val cacheControl = new CacheControl()
    cacheControl.setMustRevalidate(true)
    cacheControl.setNoCache(true)
    cacheControl.setNoStore(true)
    cacheControl
  }
  
  @GET
  @Path("ping")  
  def ping() : Response = {    
    Response.ok().cacheControl(noCache).build()
  }
  
  @GET
  @Path("health")  
  def health() : Response = {
    val rb = try {
        if(HealthCheck.check()) {
          Response.ok()
        } else {
          Response.serverError()
        }
      } catch {
        case ex : Exception =>
          logger.error("Erro em health check:" + ex, ex)
          Response.serverError()
      }
    rb.cacheControl(noCache).build()
  }
  
 /* @GET
  @Path("metrics")
  def metrics() : Response = {
    Response.ok().cacheControl(noCache).build()
  } */
  
}

object HealthCheck {
  def check() : Boolean = {    
    true
  }
}

class ParserServiceActor extends Actor with Logging {

  def receive: PartialFunction[Any, Unit] = {
    case rp: RequestProcessor =>
      logger.info("ParserServiceActor: message received")
      rp.process()
    case x =>
      logger.error("Received unexpected message:  " + x)
  }
}

