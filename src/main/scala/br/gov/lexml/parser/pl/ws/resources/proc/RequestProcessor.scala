package br.gov.lexml.parser.pl.ws.resources.proc

import java.io.File
import java.net.URI

import br.gov.lexml.parser.pl.errors.{ErroSistema, FalhaConversaoPrimaria, ParseException, ParseProblem}
import br.gov.lexml.parser.pl.ws.{CacheElem, CacheKey, Dependencies, Initializer, MimeExtensionRegistry, ServiceParams}
import br.gov.lexml.parser.pl.ws.data._
import br.gov.lexml.parser.pl.ws.tasks.Tasks
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import grizzled.slf4j.Logging

import scala.language.postfixOps
import scala.xml._
import java.time.ZonedDateTime

import br.gov.lexml.parser.pl.ws.resources.ScalaParserService
import io.prometheus.client.Counter
import io.prometheus.client.Summary



final case class RequestContext(
  resultBuilder: Seq[String] => URI,
  uniqueId: CacheKey,
  req: ParserRequisicao,
  waitId: CacheKey,
  dataHoraProcessamento: ZonedDateTime,
  fonte: Option[Array[Byte]],
  fonteFileName : Option[String])

object RequestProcessor {
  val createdFilesCount: Counter = Counter.build().name("lexml_parser_created_files_count").help("Número de arquivos criados pelo Parser").register()
  val bytesWritten: Counter = Counter.build().name("lexml_parser_written_bytes_count").help("Número de bytes gravados pelo Parser").register()
  val failureCount: Counter = Counter.build().name("lexml_parser_failure_count").help("Número de falhas no Parser")
          .labelNames("codigo_tipo_falha").register()
  val parserLatency: Summary = Summary.build().name("lexml_parser_job_latency").help("Duração da execução do parser").register()
  val srcToXhtmlLatency: Summary = Summary.build().name("lexml_parser_src_to_xhtml_latency").help("Duração da coversão da fonte em XHTML").register()
  val geracaoLatency: Summary =
    Summary.build().name("lexml_parser_geracao_latency").help("Duração de geração de saídas do parser")
      .labelNames("tipo_saida").register()

  val problemCount: Summary = Summary.build().name("parse_job_problem_count").help("Número de problemas na execução do parser")
    .labelNames("codigo_problema").register()
}
  
class RequestProcessor(ctx: RequestContext) extends Logging {
  import RequestProcessor._
  import scala.collection.mutable.{ListBuffer => MList}


  def buildKey(comps: String*): CacheKey = ctx.uniqueId ++ comps

  def buildSaidaComponents(ts: TipoSaida, fmt : FormatoSaida , tm: TipoMimeSaida, data: Array[Byte], path: String*): (ElementoSaida, Option[(CacheKey,CacheElem)]) = {
    val digest = Tasks.calcDigest(data)
    logger.debug("buildSaidaComponents: tm = " + tm)

    val (href,teso,keyContents) = fmt match {
      case FS_EMBUTIDO =>
        val xml = XML.load(new java.io.ByteArrayInputStream(data))
        (None,Some(ConteudoXML(xml)),None)
      case _ =>
        val ext = "." + MimeExtensionRegistry.mimeToExtension(tm.toString).getOrElse("txt")
        val extPath = path.init :+ (path.last ++ ext)
        val f = buildKey(extPath: _*)
        val ce = CacheElem(mimeType = tm.toString, fileName = path.last + ext, contents = data)
        (Some(ctx.resultBuilder(extPath)),None,Some((f,ce)))
    }

    val tes = ElementoSaida(
      digest = Some(digest),
      tipoSaida = ts,
      tipoMime = tm,
      href = href,
      conteudoSaida = teso
    )
    (tes, keyContents)
  }

  def fromProblem(p: ParseProblem): Falha = {
    val msg = UsuarioMensagem.mensagemUsuario(p).map(_.toString)
    Falha(
      codigoTipoFalha = p.problemType.code,
      nomeTipoFalha = p.problemType.description,
      descricao = p.toString,
      posicoes = p.pos.toVector,
      mensagemUsuario = msg)
  }

  def fromCaracteristica: ((String, Boolean)) => Caracteristica = {
    case (x: String, f: Boolean) => Caracteristica(
      descricao = x, presente = f, suportadoLexEdit = isCaracteristicaSupported(x))
  }

  def isCaracteristicaSupported(s: String): Boolean =
    !CaracteristicasImpeditivas.caracteristicaImpeditiva(s)

  def classChain(c: Class[_], l: List[String] = List()): String = c match {
    case null => l.mkString(">")
    case _ => classChain(c.getSuperclass, c.getName :: l)
  }

  def process(): Unit = try {
    logger.info("process: start")
    val falhas: MList[Falha] = MList()
    val caracteristicas: MList[Caracteristica] = MList()
    val saidas: MList[ElementoSaida] = MList()
    //val outMap: MMap[File, (String, Array[Byte])] = MMap()
    var digest: Option[String] = None

    val reqSaidas = Dependencies.completeDependencies(ctx.req).saidas.map(t => (t.tipo,t.formato)).toMap

    val resMap = Initializer.boot.get.cache.resultMap
    def geraSaida[T](ts: TipoSaida, mime: String, path: String*)(data: => Option[(Array[Byte],T)]): Option[(Array[Byte],T)] = {
      reqSaidas.get(ts) match {
        case Some(formato) =>
          val timer = geracaoLatency.labels(ts.toString).startTimer()
          try {
            logger.info("gerando saida ts = " + ts + " path = " + path)
            logger.info("mime saida = " + mime)
            data map { case (d,r) =>
              val (tes, of) = buildSaidaComponents(ts, formato, TipoMimeSaida(mime), d, path: _*)
              saidas += tes
              of foreach {
                case (key,ce) =>
                  resMap.put(key,ce)
                  createdFilesCount.inc()
                  bytesWritten.inc(ce.contents.length)
              }
              (d,r)
            }
          }  finally {
            timer.observeDuration()
          }
        case _ => None // data
      }

    }

    def geraSaidaI[T](ts: TipoSaida, mime: String, /*digest: Option[String],*/ path: String*)(data: => Option[(Array[Byte],T)]): Option[(Array[Byte],T)] = {
      try {        
        geraSaida(ts, mime, path: _*)(data)
      } catch {
        case ex: ParseException => falhas ++= ex.errors.map(fromProblem) ; None
        case ex: Exception => falhas += fromProblem(ErroSistema(ex)) ; None
      }
    }
    
    var numDiffs : Option[Int] = None
    try {
      val texto = ctx.req.textoEntrada match {
        case TE_TextoEmbutido(value) => value
        case _: TE_TextoAnexo => ctx.fonte.getOrElse(throw ParseException(TextoAnexoOmitido))
        case t => throw ParseException(TipoTextoNaoSuportado(t.getClass))
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
      val baseMimeTypeList = mimeFromExtension :+ ctx.req.tipoMime.value
      /*val baseMimeTypeList = try {
        MimeUtil.getMimeTypes(texto).toList
      } catch {
        case _: IllegalArgumentException => List(ctx.req.texto.tipoMime.toString())
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
      val mimeType2 = MimeEntrada(mimeType)
      geraSaidaI(TS_DOCUMENTO_ORIGINAL, mimeType, "original", "documento")(Some((texto,())))
      geraSaidaI(TS_PDF_ORIGINAL, "application/pdf", "original", "documento") {
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
      geraSaidaI(TS_XML_REMISSOES, "text/xml", "gerado", "remissoes") {
          Some((Tasks.makeRemissoes(xhtmlEntrada,urnContexto),()))
      }
      geraSaidaI(TS_XHTML_INTERMEDIARIO, "application/xhtml+xml", "intermediario", "documento") {
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

      lazy val oXmlBytes = geraSaida(TS_XML_DERIVADO, "text/xml", "gerado", "documento") {
        Some((xml.toString.getBytes("utf-8"),()))
      }.map(_._1)

      oXmlBytes foreach { xmlBytes =>
        geraSaidaI(TS_ZIP_DERIVADO, "application/zip", "gerado", "documento") {
          Some((Tasks.makeLexMLZip(pl, xmlBytes),()))
        }
        geraSaidaI(TS_PDF_DERIVADO, "application/pdf", "gerado", "documento") {
          Some((Tasks.renderPDF(xmlBytes),()))
        }
        geraSaidaI(TS_DOCX_DERIVADO, "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "gerado", "documento") {
          Some((Tasks.renderDOCX(xmlBytes),()))
        }.map(_._1)
        
        val docxParaDiff = geraSaidaI(TS_DOCXDIFF_DERIVADO, "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "gerado", "documento") {
          Some((Tasks.renderDOCXDIFF(xmlBytes),()))
        }.map(_._1)     
        numDiffs = docxParaDiff.flatMap(docx => geraSaidaI(TS_PDF_DIFF, "application/pdf", "gerado", "diff") {
          Tasks.buildDiff(texto, mimeType,docx,"application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        }).flatMap(_._2)
      }
    } catch {
      case ex: ParseException =>
        falhas ++= ex.errors.map(fromProblem)
      case ex: Exception =>
        logger.error("Exception in request processor(" + classChain(ex.getClass) + ") : " + ex.getMessage, ex)
        falhas += fromProblem(ErroSistema(ex))
    }

    logger.info("Falhas: " + falhas)    
    falhas.foreach(f => failureCount.labels(f.nomeTipoFalha).inc())
    
    val ps = ParserResultado(
      metadado = ctx.req.metadado,
      falhas = falhas.toVector,
      caracteristicas = caracteristicas.toVector,
      saidas = saidas.toVector,
      digestFonte = digest.getOrElse(""),
      dataHoraProcessamento = ctx.dataHoraProcessamento,
      numeroDiferencas = numDiffs,
      componentes = ServiceParams.configuracao)

    val psXml = ScopeHelper.removeEmptyNsNodeSeq(ps.asXML)
    val psXmlTxt = psXml.toString
    val resXml = """<?xml-stylesheet type="text/xsl" href="../../static/resultado2xhtml.xsl"?>""" + psXmlTxt

    logger.debug("writing outputs")
    resMap.put(ctx.uniqueId + "resultado.xml", CacheElem(mimeType="text/xml",fileName="resultado.xml",contents=resXml.getBytes("utf-8")))
  } finally {
    logger.debug("deleting wait file")
    val resMap = Initializer.boot.get.cache.resultMap
    resMap.delete(ctx.waitId)
    ScalaParserService.parserJobsInProgress.dec()
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
