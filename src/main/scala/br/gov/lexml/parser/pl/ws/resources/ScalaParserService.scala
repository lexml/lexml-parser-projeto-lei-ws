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
import br.gov.lexml.parser.pl.ws.data.scalaxb.{ParserRequisicao}
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
import scalaxb_1_1.fromXMLEither

//import org.apache.commons.codec.binary.Base64

@Path("/parse")
class ScalaParserService extends Logging {
  
  @Resource
  var context : WebServiceContext = _

  @Context
  var uriInfo: UriInfo = _

  private def buildURIFromRelativePath(path: String*) = uriInfo match {
    case null ⇒ new URI("http://localhost:8180/parser" + path.mkString(";", ";", ""))
    case _ ⇒
      val builder = uriInfo.getBaseUriBuilder
      builder.scheme("https")
      for (c ← path) builder.path(c)
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

  /*  @GET
  @Path("parseSenado") 
  @Produces(Array("text/html"))  
  def getParsePage() : InputStream =
    Response.temporaryRedirect()
  	getClass.getClassLoader.getResourceAsStream("pages/parserSimulador.html")
*/
  /*
  @GET
  @Path("parseSenado/{page: .+}") //@Produces(Array("application/html"))  
  def getParsePages(@PathParam("page") page: String): InputStream =
    getClass.getClassLoader.getResourceAsStream("pages/" + page)
  */

  @GET
  @Path("result/{id}")
  @Produces(Array("text/xml"))
  def readResultGet(@PathParam("id") id: String): Response = {
    logger.info("readResultGet: id = " + id)    
    Response.temporaryRedirect(uriInfo.getRequestUriBuilder.path("resultado.xml").build()).build
  }

  
  /*  @GET
  @Path("result/{id}")
  @Produces(Array("text/html"))
  def readResultGetHtml(@PathParam("id") id : String) = touchId(id) {
    logger.info("readResultGetHtml: id = " + id)
    doReadResult2(id,"resultado.xml") match {
      case Some((file,"text/xml")) => {
        logger.info("readResultGetHtml: xml retrieved. applying transform")
        val s = new StreamSource(new BufferedInputStream(new FileInputStream(file)))
        val tf = newTransform()        
        val bos = new java.io.ByteArrayOutputStream()
        val res = new javax.xml.transform.stream.StreamResult(bos)
        newTransform().transform(s,res)
        logger.info("readResultGetHtml: transform applied, returning result")
        Response.ok(bos.toByteArray(),"application/xhtml+xml").build()
      }
      case None => {
        logger.info("readResultGetHtml: result not found!")
        notFound
      }
    }        
  }*/

  /*@GET
  @Path("result/{id}")
  @Produces(Array("text/html"))	  	
  def readResult(@PathParam("id") id : String) : Response = touchId(id) {
    doReadResult2(id,"resultado.xml") match {
      case None => Response.status(Response.Status.NOT_FOUND).build()
      case Some((resFile,_)) => {
        val xml = XML.loadFile(resFile.getPath())
        scalaxb_1_1.fromXMLEither[ParserResultado](xml) match {
          case Left(err) => {
            logger.error("Erro na leitura de resultado. id = " + id + ": " + err)
            Response.status(Response.Status.INTERNAL_SERVER_ERROR).build()
          }
          case Right(resultado) => {
            val xml = geraHtmlResultado(resultado)
            Response.ok(xml,MediaType.APPLICATION_XHTML_XML).build()
          }
        }
      }
    }
  }*/

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
    val system = boot.system
    touchSession()
    try {
      fromXMLEither[ParserRequisicao](
        XML.loadString(requisicaoText)) match {
          case Left(error) ⇒
            logger.warn("Requisicao invalida: " + error + "\ninput: " + requisicaoText)
            Response.status(Response.Status.BAD_REQUEST).build()

          case Right(requisicao) ⇒
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
            val dataHoraProcessamento = javax.xml.datatype.DatatypeFactory.newInstance.newXMLGregorianCalendar(new java.util.GregorianCalendar())
            logger.info("sending message to actor with uniqueId=%s, requisicao=%s, resultPath=%s, waitFile=%s, dataHoraProcessamento=%s".format(uniqueId, requisicao, resultPath, waitFile, dataHoraProcessamento))
            boot.parserServiceRouter ! new RequestProcessor(RequestContext(resultURI, uniqueId, requisicao, resultPath, waitFile, dataHoraProcessamento, fonte, fileName))
            val uri = resultURI()
            logger.info("result for " + uniqueId + ", at  " + uri)
            Response.created(uri).entity(<Location>{ uri.toURL.toExternalForm }</Location>.toString).build()

        }
    } catch {
      case ex: Exception ⇒
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
    pathComps match {
      case Seq("resultado2xhtml.xsl") ⇒
        Response.ok(getResultado2XhtmlData, "application/xslt+xml").build()

      case _ ⇒ doReadResult2(id, pathComps: _*).map(r ⇒ Response.ok(r._1, r._2))
        .getOrElse(notFoundBuilder).build()
    }
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
      val mimeType = FileUtils.readFileToString(mimeFile)
      logger.info("responding to get with reqFile = " + reqFile + ", and mimeType = " + mimeType)
      Some((reqFile, mimeType))
    }
  }
  val idRegex: Regex = "^[A-Z0-9]+$"r
  def touchId(id: String)(rest: ⇒ Response): Response = {
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
    import scala.collection.JavaConversions._
    def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
    val availableMimes = MimeUtil.
      getMimeTypes(f).
      toList
    val accepted = if (filterAccept) { availableMimes.filter(accept) } else { availableMimes }
    if (accepted.isEmpty) {
      None
    } else {
      Some(MimeUtil.getMostSpecificMimeType(accepted).toString)
    }
  }
  
  private def getMostSpecificMimeType(b : Array[Byte], filterAccept: Boolean): Option[String] = {
    import scala.collection.JavaConversions._
    def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
    val availableMimes = MimeUtil.
      getMimeTypes(b).
      toList
    val accepted = if (filterAccept) { availableMimes.filter(accept) } else { availableMimes }
    if (accepted.isEmpty) {
      None
    } else {
      Some(MimeUtil.getMostSpecificMimeType(accepted).toString)
    }
  }

  @Path("static/{file: .*}")
  @GET
  def serveStatic(@PathParam("file") file: String): Response = {
    logger.info("serveStatic: file = " + file)
    val staticDir = ServiceParams.params.staticOverrideDirectory
    val f = new File(new File(staticDir, "lexml-static"),file).getCanonicalFile
    logger.info("staticDir: " + staticDir)
    logger.info("serving: " + f)
    logger.info("f.exists = " + f.exists + ", f.isFile = " + f.isFile)
    val noFilterAccept = false
    if (f.getPath.startsWith(staticDir.getPath) && f.exists() && f.isFile) {

      val mt = getMostSpecificMimeType(f, noFilterAccept)
      logger.info("serveStatic: found on static dir. f = " + f + ", mt = " + mt)
      Response.ok(f, mt.getOrElse("application/binary")).header("Cache-control", "no-cache").build()
    } else {
      Option(this.getClass.getResourceAsStream("lexml-static/" + file)) match {
        case None ⇒
          logger.info("serveStatic: not found.")
          notFound
        case Some(is) ⇒
          val d = IOUtils.toByteArray(is)
          val mt = getMostSpecificMimeType(d, noFilterAccept)
          logger.info("serveStatic: found on classpath. mt = " + mt)
          Response.ok(d, mt.getOrElse("application/binary")).header("Cache-control", "no-cache").build()

      }
    }
  }
}

class ParserServiceActor extends Actor with Logging {

  def receive: PartialFunction[Any, Unit] = {
    case rp: RequestProcessor ⇒
      logger.info("ParserServiceActor: message received")
      rp.process()
    case x ⇒
      logger.error("Received unexpected message:  " + x)
  }
}

