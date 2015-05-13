package br.gov.lexml.parser.pl.ws.resources.proc

import java.io.File
import java.net.URI
import org.apache.commons.io.FileUtils
import grizzled.slf4j.Logging
import br.gov.lexml.parser.pl.errors.ParseException
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.ws.data.scalaxb._
import br.gov.lexml.parser.pl.ws.ServiceParams
import javax.xml.datatype.XMLGregorianCalendar
import br.gov.lexml.parser.pl.errors.TErroSistema
import br.gov.lexml.parser.pl.ws.tasks.Tasks
import br.gov.lexml.parser.pl.errors.ErroSistema
import br.gov.lexml.parser.pl.ws.Mime
import eu.medsea.mimeutil.MimeUtil
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import eu.medsea.mimeutil.MimeType
import br.gov.lexml.parser.pl.errors.FalhaConversaoPrimaria
import scala.xml.XML
import scala.xml.Elem


final case class RequestContext(
  resultBuilder: (String*) ⇒ URI,
  uniqueId: String,
  req: ParserRequisicao,
  resultPath: File,
  waitFile: File,
  dataHoraProcessamento: XMLGregorianCalendar,
  fonte: Option[Array[Byte]],
  fonteFileName : Option[String])

class RequestProcessor(ctx: RequestContext) extends Logging {
  import scala.collection.mutable.{ Map ⇒ MMap, ListBuffer ⇒ MList }

  type OutputFileMap = Map[File, (String, Array[Byte])]

  def buildPath(comps: String*) = comps.foldLeft(ctx.resultPath)(new File(_, _))

  def buildSaidaComponents(ts: TipoSaida, fmt : TipoFormatoSaida , tm: TipoMimeSaida, data: Array[Byte], path: String*) = {
    val digest = Tasks.calcDigest(data)
    logger.debug("buildSaidaComponents: tm = " + tm)
        
    val (href,teso,f) = fmt match {
      case EMBUTIDO => {
        val xml = XML.load(new java.io.ByteArrayInputStream(data))
        (None,Some(scalaxb.DataRecord(xml)),None)
      }
      case _ => {
        val ext = "." + Mime.mimeToExtension(tm.toString).getOrElse("txt")
        val extPath = path.init :+ (path.last ++ ext)
        val f = buildPath(extPath: _*)
        (Some(ctx.resultBuilder(extPath: _*)),None,Some(f))
      }
    } 
    val tes = TipoElementoSaida(Some(TipoDigest(digest)), teso, ts, tm, href)    
    (tes, f)
  }

  def fromProblem(p: ParseProblem): TipoFalha = {
    val msg = UsuarioMensagem.mensagemUsuario(p).map(_.toString)
    TipoFalha(p.problemType.code, p.problemType.description, p.pos, p.message, msg)
  }

  def fromCaracteristica: ((String, Boolean)) ⇒ TipoCaracteristica = {
    case (x: String, f: Boolean) ⇒ TipoCaracteristica(x, f, isCaracteristicaSupported(x))
  }

  def isCaracteristicaSupported(s: String) =
    !CaracteristicasImpeditivas.caracteristicaImpeditiva(s)

  def classChain(c: Class[_], l: List[String] = List()): String = c match {
    case null ⇒ l.mkString(">")
    case _ ⇒ classChain(c.getSuperclass, c.getName :: l)
  }

  def process() = try {
    logger.info("process: start")
    val falhas: MList[TipoFalha] = MList()
    val caracteristicas: MList[TipoCaracteristica] = MList()
    val saidas: MList[TipoElementoSaida] = MList()
    val outMap: MMap[File, (String, Array[Byte])] = MMap()
    var digest: Option[String] = None

    def geraSaida[T](ts: TipoSaida, mime: String, digest: Option[String], path: String*)(data: ⇒ Option[(Array[Byte],T)]): Option[(Array[Byte],T)] = {
      ctx.req.saidas.tipoSaida.filter(_.tipo == ts).headOption match {
        case Some(tts) ⇒ {
          logger.info("gerando saida ts = " + ts + " path = " + path)
          logger.info("mime saida = " + mime)
          data map { case (d,r) =>             
            val (tes, of) = buildSaidaComponents(ts, tts.formato, TipoMimeSaida.fromString(mime), d, path: _*)          
            saidas += tes
            of foreach (f => outMap += (f -> (mime, d)))          
            (d,r)
          }
        }
        case _ => data
      }
      
    }

    def geraSaidaI[T](ts: TipoSaida, mime: String, digest: Option[String], path: String*)(data: ⇒ Option[(Array[Byte],T)]): Option[(Array[Byte],T)] =
      try {
        geraSaida(ts, mime, digest, path: _*)(data)        
      } catch {
        case ex: ParseException ⇒ falhas ++= ex.errors.map(fromProblem(_)) ; None
        case ex: Exception ⇒ falhas += fromProblem(ErroSistema(ex)) ; None
      }

    var numDiffs : Option[Int] = None
    try {
      val texto = ctx.req.texto.tipotextooption.value match {
        case TipoTextoEmbutido(value) ⇒ value.toArray[Byte]
        case _: TipoTextoAnexo ⇒ ctx.fonte.getOrElse(throw new ParseException(TextoAnexoOmitido))
        case t ⇒ throw new ParseException(TipoTextoNaoSuportado(t.getClass))
      }
      val hash = Tasks.calcDigest(texto)
      digest = Some(hash)
      val metadado = Tasks.buildMetadado(ctx.req.metadado, hash)
      import scala.collection.JavaConversions._
      
      def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
      val mimeFromExtension = ctx.fonteFileName.toList.flatMap { fname =>
        val idx = fname.lastIndexOf('.')
        val ext = if(idx >= 0) {
          Some(fname.substring(idx+1))
        } else {
          None
        }
        ext.toList.flatMap(Mime.ext2mime.get(_)).flatten
      }
      val baseMimeTypeList = mimeFromExtension :+ ctx.req.texto.tipoMime.toString 
      /*val baseMimeTypeList = try {
        MimeUtil.getMimeTypes(texto).toList
      } catch {
        case _: IllegalArgumentException ⇒ List(ctx.req.texto.tipoMime.toString())
      }*/
      //      println("baseMimeTypeList = " + baseMimeTypeList)
      logger.info("baseMimeTypeList = " + baseMimeTypeList + ", accept = " + XHTMLProcessor.accept)
      val possibleMimeTypes = baseMimeTypeList.filter(accept)
      if (possibleMimeTypes.isEmpty) {
        throw new ParseException(FalhaConversaoPrimaria)
      }
      //      println("Possible mime types: " + possibleMimeTypes)
      val mimeType = possibleMimeTypes.head //MimeUtil.getMostSpecificMimeType(possibleMimeTypes).toString
      logger.info("mimeType = " + mimeType)
      val mimeType2 = TipoMimeEntrada.fromString(mimeType)
      geraSaidaI(DOCUMENTO_ORIGINAL, mimeType, Some(hash), "original", "documento")(Some((texto,())))
      geraSaidaI(PDF_ORIGINAL, "application/pdf", None, "original", "documento") {        
        Some((Tasks.docToPDF(texto, mimeType2),()))
      }
      val xhtmlEntrada = Tasks.srcToXhtmlTask(texto, mimeType2)

      val (parseResult, problems) = Tasks.parse(metadado, xhtmlEntrada, ctx.req.opcoes)
      logger.debug("problems = " + problems)
      val (pl, xml) = parseResult.getOrElse(throw new ParseException(problems: _*))
      problems.foreach(falhas += fromProblem(_))
      pl.caracteristicas.foreach(caracteristicas += fromCaracteristica(_))
      lazy val xmlBytes = geraSaida(XML_DERIVADO, "text/xml", None, "gerado", "documento") {
        Some((xml.toString.getBytes("utf-8"),()))
      }.get._1
      geraSaidaI(ZIP_DERIVADO, "application/zip", None, "gerado", "documento") {
        Some((Tasks.makeLexMLZip(pl, xmlBytes),()))
      }
      geraSaidaI(PDF_DERIVADO, "application/pdf", None, "gerado", "documento") {
        Some((Tasks.renderPDF(xmlBytes, metadado),()))
      }
      val rtfDerivado = geraSaidaI(RTF_DERIVADO, "text/rtf", None, "gerado", "documento") {
        Some((Tasks.renderRTF(xmlBytes, metadado),()))
      }.map(_._1)
            
     /* geraSaidaI(EPUB_DERIVADO, "application/epub+zip", None, "gerado", "documento") {
        Tasks.renderEPUB(xmlBytes,metadado)
      }*/
      
      numDiffs = rtfDerivado.flatMap(rtf => geraSaidaI(PDF_DIFF, "application/pdf", None, "gerado", "diff") {
        Tasks.buildDiff(texto, mimeType,rtf,"text/rtf")
      }).flatMap(_._2)
    } catch {
      case ex: ParseException ⇒ {
        falhas ++= ex.errors.map(fromProblem(_))
      }
      case ex: Exception ⇒ {
        logger.error("Exception in request processor(" + classChain(ex.getClass) + ") : " + ex.getMessage, ex)
        falhas += fromProblem(ErroSistema(ex))
      }
    }

    logger.info("Falhas: " + falhas)
    val res = TipoResultado(
      falhas = TipoFalhas(falhas: _*),
      caracteristicas = TipoCaracteristicas(caracteristicas: _*),
      saidas = TipoSaidasResultado(saidas: _*),
      digestFonte = TipoDigest(digest.getOrElse("")),
      dataHoraProcessamento = ctx.dataHoraProcessamento,
      numeroDiferencas = numDiffs)

    val ps = ParserResultado(ctx.req.metadado, res, ServiceParams.configuracao)
    
    val psXml = scalaxb.toXML[ParserResultado](ps, None, Some("ParserResultado"), defaultScope)
    val resXml = (<?xml-stylesheet type="text/xsl" href="../../static/resultado2xhtml.xsl"?> +: psXml)
    
    logger.debug("writing outputs")
    writeOutputs(outMap.toMap)
    writeOutputs(Map(buildPath("resultado.xml") -> ("text/xml", resXml.toString.getBytes("utf-8"))))
  } finally {
    logger.debug("deleting wait file")
    ctx.waitFile.delete()
  }

  def writeOutputs(m: OutputFileMap) =
    for { (f, (mime, data)) ← m } {
      logger.debug("writing " + f)
      FileUtils.forceMkdir(f.getParentFile())
      FileUtils.writeByteArrayToFile(f, data)
      val mf = new File(f.getParentFile(), f.getName() + ".mime")
      FileUtils.writeStringToFile(mf, mime)      
    }
}