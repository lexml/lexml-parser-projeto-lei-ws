package br.gov.lexml.parser.pl.ws.resources.proc

import java.io.File
import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar

import br.gov.lexml.parser.pl.errors.{ErroSistema, FalhaConversaoPrimaria, ParseException, ParseProblem}
import br.gov.lexml.parser.pl.ws.{Dependencies, MimeExtensionRegistry, ServiceParams}
import br.gov.lexml.parser.pl.ws.data.scalaxb._
import br.gov.lexml.parser.pl.ws.tasks.Tasks
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import grizzled.slf4j.Logging
import org.apache.commons.io.FileUtils

import scala.language.postfixOps
import scala.xml._
import java.nio.charset.Charset
import br.gov.lexml.parser.pl.ws.resources.ScalaParserService
import io.prometheus.client.Counter
import io.prometheus.client.Summary



final case class RequestContext(
  resultBuilder: Seq[String] ⇒ URI,
  uniqueId: String,
  req: ParserRequisicao,
  resultPath: File,
  waitFile: File,
  dataHoraProcessamento: XMLGregorianCalendar,
  fonte: Option[Array[Byte]],
  fonteFileName : Option[String])

object RequestProcessor {
  val createdFilesCount: Counter = Counter.build().name("lexml_parser_created_files_count").help("Número de arquivos criados no sistema de arquivos pelo Parser").register()
  val bytesWritten: Counter = Counter.build().name("lexml_parser_written_bytes_count").help("Número de arquivos criados no sistema de arquivos pelo Parser").register()
  val failureCount: Counter = Counter.build().name("lexml_parser_failure_count").help("Número de falhas no Parser")
          .labelNames("codigo_tipo_falha").register()
  val parserLatency: Summary = Summary.build().name("lexml_parser_job_latency").help("Duração da execução do parser").register()
  val srcToXhtmlLatency: Summary = Summary.build().name("lexml_parser_src_to_xhtml_latency").help("Duração da coversão da fonte em XHTML").register()
  val geracaoLatency: Summary =
    Summary.build().name("lexml_parser_geracao_latency").help("Duração de geração de saídas do parser")
      .labelNames("tipo_saida").register()
/*  private var _geracaoLatency : Map[TipoSaida,Summary] = Map()
  def geracaoLatency(ts : TipoSaida) : Summary = synchronized {
    _geracaoLatency.get(ts) match {
      case Some(x) => x
      case None =>
        val x = Summary.build().name("parse_job_geracao_latency_" + ts.toString.toLowerCase)
            .help("Latência na geração de saída de tipo " + ts.toString).register()
        _geracaoLatency = _geracaoLatency + (ts -> x)
        x
    }
  } */
  
  /* private var _falhaCounts : Map[Int,Counter] = Map()
  def falhaCount(tf : TipoFalha) : Counter = synchronized {
    _falhaCounts.get(tf.codigoTipoFalha) match {
      case Some(x) => x
      case None =>
        val x = Counter.build().name("parse_job_falha_" + tf.nomeTipoFalha.toLowerCase)
            .help("Número de falhas de tipo " + tf.nomeTipoFalha.toString).register()
        _falhaCounts = _falhaCounts + (tf.codigoTipoFalha -> x)
        x
    }
  }  */
  
  val problemCount: Summary = Summary.build().name("parse_job_problem_count").help("Número de problemas na execução do parser")
    .labelNames("codigo_problema").register()
}
  
class RequestProcessor(ctx: RequestContext) extends Logging {
  import RequestProcessor._
  import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

  type OutputFileMap = Map[File, (String, Array[Byte])]

  def buildPath(comps: String*): File = comps.foldLeft(ctx.resultPath)(new File(_, _))



  def buildSaidaComponents(ts: TipoSaida, fmt : TipoFormatoSaida , tm: TipoMimeSaida, data: Array[Byte], path: String*): (TipoElementoSaida, Option[File]) = {
    val digest = Tasks.calcDigest(data)
    logger.debug("buildSaidaComponents: tm = " + tm)

    val (href,teso,f) = fmt match {
      case EMBUTIDO =>
        val xml =XML.load(new java.io.ByteArrayInputStream(data))
        (None,Some(scalaxb.DataRecord.fromAny(xml, x => throw new RuntimeException(s"Unexpected unhandled: $x"))),None)
      case _ =>
        val ext = "." + MimeExtensionRegistry.mimeToExtension(tm.toString).getOrElse("txt")
        val extPath = path.init :+ (path.last ++ ext)
        val f = buildPath(extPath: _*)
        (Some(ctx.resultBuilder(extPath)),None,Some(f))
    }
    import scalaxb._
    import br.gov.lexml.parser.pl.ws.data.scalaxb._
    val tes = TipoElementoSaida(
      digest = Some(TipoDigest(digest)),
      tipoelementosaidaoption  = teso.map(x => DataRecord(x)),
      attributes = Map(
        "@tipoSaida" -> DataRecord(ts),
        "@tipoMime" -> DataRecord(tm)) ++
        href.map(uri => ("@href",DataRecord(uri)))
    )
    /*
    (digest: Option[br.gov.lexml.parser.pl.ws.data.scalaxb.TipoDigest] = None,
  tipoelementosaidaoption: Option[scalaxb.DataRecord[Any]] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val tipoSaida = attributes("@tipoSaida").as[TipoSaida]
  lazy val tipoMime = attributes("@tipoMime").as[TipoMimeSaida]
  lazy val href = attributes.get("@href") map { _.as[java.net.URI]}
}
     */
    (tes, f)
  }

  def fromProblem(p: ParseProblem): TipoFalha = {
    val msg = UsuarioMensagem.mensagemUsuario(p).map(_.toString)
    TipoFalha(p.problemType.code, p.problemType.description, p.pos, p.message, msg)
  }

  def fromCaracteristica: ((String, Boolean)) ⇒ TipoCaracteristica = {
    case (x: String, f: Boolean) ⇒ TipoCaracteristica(x, f, isCaracteristicaSupported(x))
  }

  def isCaracteristicaSupported(s: String): Boolean =
    !CaracteristicasImpeditivas.caracteristicaImpeditiva(s)

  def classChain(c: Class[_], l: List[String] = List()): String = c match {
    case null ⇒ l.mkString(">")
    case _ ⇒ classChain(c.getSuperclass, c.getName :: l)
  }

  def process(): Unit = try {
    logger.info("process: start")
    val falhas: MList[TipoFalha] = MList()
    val caracteristicas: MList[TipoCaracteristica] = MList()
    val saidas: MList[TipoElementoSaida] = MList()
    val outMap: MMap[File, (String, Array[Byte])] = MMap()
    var digest: Option[String] = None

    val reqSaidas = Dependencies.completeDependencies(ctx.req).saidas.tipoSaida.map(t => (t.tipo,t.formato)).toMap

    def geraSaida[T](ts: TipoSaida, mime: String, path: String*)(data: ⇒ Option[(Array[Byte],T)]): Option[(Array[Byte],T)] = {
      reqSaidas.get(ts) match {
        case Some(formato) ⇒
          val timer = geracaoLatency.labels(ts.toString).startTimer()
          try {
            logger.info("gerando saida ts = " + ts + " path = " + path)
            logger.info("mime saida = " + mime)
            data map { case (d,r) =>
              val (tes, of) = buildSaidaComponents(ts, formato, TipoMimeSaida.fromString(mime,defaultScope), d, path: _*)
              saidas += tes
              of foreach (f => outMap += (f -> (mime, d)))
              (d,r)
            }
          }  finally {
            timer.observeDuration()
          }
        case _ => None // data
      }

    }

    def geraSaidaI[T](ts: TipoSaida, mime: String, /*digest: Option[String],*/ path: String*)(data: ⇒ Option[(Array[Byte],T)]): Option[(Array[Byte],T)] = {
      try {        
        geraSaida(ts, mime, path: _*)(data)
      } catch {
        case ex: ParseException ⇒ falhas ++= ex.errors.map(fromProblem) ; None
        case ex: Exception ⇒ falhas += fromProblem(ErroSistema(ex)) ; None
      }
    }
    
    var numDiffs : Option[Int] = None
    try {
      val texto = ctx.req.texto.tipotextooption.value match {
        case TipoTextoEmbutido(value) ⇒ value.toArray[Byte]
        case _: TipoTextoAnexo ⇒ ctx.fonte.getOrElse(throw ParseException(TextoAnexoOmitido))
        case t ⇒ throw ParseException(TipoTextoNaoSuportado(t.getClass))
      }
      val hash = Tasks.calcDigest(texto)
      digest = Some(hash)
      val metadado = Tasks.buildMetadado(ctx.req.metadado, hash)
      val urnContexto = metadado.urnContextoLinker        
        
      
      def accept(m: Any) = XHTMLProcessor.accept.contains(m.toString)
      val mimeFromExtension = ctx.fonteFileName.toList.flatMap { fname =>
        val idx = fname.lastIndexOf('.')
        val ext = if(idx >= 0) {
          Some(fname.substring(idx+1))
        } else {
          None
        }
        ext.toList.flatMap(MimeExtensionRegistry.ext2mime.get).flatten
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
        throw ParseException(FalhaConversaoPrimaria)
      }
      //      println("Possible mime types: " + possibleMimeTypes)
      val mimeType = possibleMimeTypes.head //MimeUtil.getMostSpecificMimeType(possibleMimeTypes).toString
      logger.info("mimeType = " + mimeType)
      val mimeType2 = TipoMimeEntrada.fromString(mimeType,defaultScope)
      geraSaidaI(DOCUMENTO_ORIGINAL, mimeType, "original", "documento")(Some((texto,())))
      geraSaidaI(PDF_ORIGINAL, "application/pdf", "original", "documento") {
        Some((Tasks.docToPDF(texto, mimeType2),()))
      }
      val xhtmlEntrada = {
        val timer = srcToXhtmlLatency.startTimer()
        try { 
          Tasks.srcToXhtmlTask(texto, mimeType2)
        } finally {
          timer.observeDuration()
        }
      }
      geraSaidaI(XML_REMISSOES, "text/xml", "gerado", "remissoes") {
          Some((Tasks.makeRemissoes(xhtmlEntrada,urnContexto),()))
      }
      geraSaidaI(XHTML_INTERMEDIARIO, "application/xhtml+xml", "intermediario", "documento") {
        val xhtmlDoc =
          <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
        	<head>
                 <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                 <title>XHTML INTERMEDIARIO</title></head>
        		<body>{NodeSeq fromSeq xhtmlEntrada}</body>
             </html>

        Some(( xhtmlDoc.toString.getBytes("utf-8"),()))
      }

      val (parseResult, problems) = {
        val timer = parserLatency.startTimer()
        try {      
          Tasks.parse(metadado, xhtmlEntrada, ctx.req.opcoes)
        } finally {
          timer.observeDuration()
        }
      } 
           
      logger.debug("problems = " + problems)
      
      problems.groupBy(_.problemType).view.mapValues(_.size).foreach {
        case (pt,_) => problemCount.labels(pt.description).observe(problems.length)
      }
      
      val (pl, xml) = parseResult.getOrElse(throw ParseException(problems: _*))

      problems.foreach(falhas += fromProblem(_))

      pl.caracteristicas.foreach(caracteristicas += fromCaracteristica(_))

      lazy val oXmlBytes = geraSaida(XML_DERIVADO, "text/xml", "gerado", "documento") {
        Some((xml.toString.getBytes("utf-8"),()))
      }.map(_._1)

      oXmlBytes foreach { xmlBytes =>
        geraSaidaI(ZIP_DERIVADO, "application/zip", "gerado", "documento") {
          Some((Tasks.makeLexMLZip(pl, xmlBytes),()))
        }
        geraSaidaI(PDF_DERIVADO, "application/pdf", "gerado", "documento") {
          Some((Tasks.renderPDF(xmlBytes, metadado),()))
        }
        geraSaidaI(DOCX_DERIVADO, "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "gerado", "documento") {
          Some((Tasks.renderDOCX(xmlBytes),()))
        }.map(_._1)
        
        val docxParaDiff = geraSaidaI(DOCXDIFF_DERIVADO, "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "gerado", "documento") {
          Some((Tasks.renderDOCXDIFF(xmlBytes),()))
        }.map(_._1)     
        numDiffs = docxParaDiff.flatMap(docx => geraSaidaI(PDF_DIFF, "application/pdf", "gerado", "diff") {
          Tasks.buildDiff(texto, mimeType,docx,"application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        }).flatMap(_._2)
      }
    } catch {
      case ex: ParseException ⇒
        falhas ++= ex.errors.map(fromProblem)
      case ex: Exception ⇒
        logger.error("Exception in request processor(" + classChain(ex.getClass) + ") : " + ex.getMessage, ex)
        falhas += fromProblem(ErroSistema(ex))
    }

    logger.info("Falhas: " + falhas)    
    falhas.foreach(f => failureCount.labels(f.nomeTipoFalha).inc())
    
    val res = TipoResultado(
      falhas = TipoFalhas(falhas.to(Seq)),
      caracteristicas = TipoCaracteristicas(caracteristicas.to(Seq)),
      saidas = TipoSaidasResultado(saidas.to(Seq)),
      digestFonte = TipoDigest(digest.getOrElse("")),
      dataHoraProcessamento = ctx.dataHoraProcessamento,
      numeroDiferencas = numDiffs)

    val ps = ParserResultado(ctx.req.metadado, res, ServiceParams.configuracao)

    val psXml = ScopeHelper.removeEmptyNsNodeSeq(scalaxb.toXML[ParserResultado](ps, None, Some("ParserResultado"), defaultScope))
    val psXmlTxt = psXml.toString
    val resXml = """<?xml-stylesheet type="text/xsl" href="../../static/resultado2xhtml.xsl"?>""" + psXmlTxt

    logger.debug("writing outputs")
    writeOutputs(outMap.toMap)
    writeOutputs(Map[File,(String,Array[Byte])](buildPath("resultado.xml") -> ("text/xml", resXml.getBytes("utf-8"))))
  } finally {
    logger.debug("deleting wait file")    
    ctx.waitFile.delete()
    ScalaParserService.parserJobsInProgress.dec()
  }

  def writeOutputs(m: OutputFileMap): Unit =
    for { (f, (mime, data)) ← m } {
      logger.debug("writing " + f)
      FileUtils.forceMkdir(f.getParentFile)
      FileUtils.writeByteArrayToFile(f, data)
      createdFilesCount.inc()
      bytesWritten.inc(data.length)
      val mf = new File(f.getParentFile, f.getName + ".mime")      
      FileUtils.writeStringToFile(mf, mime, Charset.forName("utf-8"))      
    }
  
  object ScopeHelper {
    type ScopeMap = Map[Option[String],Option[String]]
    val emptyScopeMap : ScopeMap = Map()
    
    def scopeToMap(ns : NamespaceBinding) : ScopeMap = {
      if(ns == null) {
        Map() 
      } else if (ns.prefix == null && ns.uri == null) {
        scopeToMap(ns.parent)
      } else {
        scopeToMap(ns.parent) + (Option(ns.prefix) -> Option(ns.uri))
      }
    }
    
    def mapToScope(m : ScopeMap) : NamespaceBinding = {      
        m.foldLeft[NamespaceBinding](TopScope) { 
          case (parent, (okey,ouri)) => 
             NamespaceBinding(okey orNull, ouri orNull,parent) 
        }
    }
      
    def fixScope(e : Elem, m : ScopeMap = emptyScopeMap) : Elem = {
      val scopeMap = m ++ scopeToMap(e.scope)
      val childs = e.child.map { case e : Elem => fixScope(e,scopeMap) ; case n => n }
      e copy (scope = mapToScope(scopeMap), child = childs)
    }
    
    def fixScopeNodeSeq(ns : NodeSeq, scopeMap : ScopeMap = emptyScopeMap) : NodeSeq =  {
      ns.map { case e : Elem => fixScope(e,scopeMap) ; case n => n }
    }
    
    def removeEmptyNs(ns : NamespaceBinding) : NamespaceBinding = { 
       if (ns == null) { ns } 
       else if (ns.prefix == null && ns.uri == null ) {
         removeEmptyNs(ns.parent) 
       }
       else { ns copy (parent = removeEmptyNs(ns.parent)) }       
    }
     
    def removeEmptyNs(e : Elem) : Elem = {
        val newScope = Option(removeEmptyNs(e.scope)).getOrElse(TopScope)        
        e copy (scope = newScope, child = e.child.map { case ee : Elem => removeEmptyNs(ee) ; case n => n })
    }
    
    def removeEmptyNsNodeSeq(ns : NodeSeq) : NodeSeq = ns.map { case ee : Elem => removeEmptyNs(ee) ; case n => n }
  }
  
}
