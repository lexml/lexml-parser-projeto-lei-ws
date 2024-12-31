package br.gov.lexml.parser.pl.ws.tasks

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.security.MessageDigest

import br.gov.lexml.parser.pl.{ProjetoLei, ProjetoLeiParser}
import br.gov.lexml.parser.pl.block.{Block, Paragraph}
import br.gov.lexml.parser.pl.errors.{ParseException, ParseProblem}
import br.gov.lexml.parser.pl.linker.Linker
import br.gov.lexml.parser.pl.metadado.{Id, Metadado}
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.profile.{DocumentProfile, DocumentProfileOverride, DocumentProfileRegister}
import br.gov.lexml.parser.pl.ws.{LexmlWsConfig, MimeExtensionRegistry}
import br.gov.lexml.parser.pl.ws.data.{OpcoesRequisicao, Metadado => RMetadado, MimeEntrada}
import br.gov.lexml.parser.pl.ws.data.{RemissaoDocumento, RemissaoFragmento, Remissoes, XT_Simple}
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor.pipeline
import br.gov.lexml.parser.pl.xhtml.{DOCXConverter, AbiwordConverter}
import br.gov.lexml.renderer.pdf.{PDFConfigs, RendererPDF}
import br.gov.lexml.renderer.strategies.XhtmlRenderer
import com.typesafe.config.Config
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.FileUtils

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq, Text}
import grizzled.slf4j.Logging
import br.gov.lexml.renderer.docx.renderers.Constants

object Tasks extends Logging {
  
  val config: Config = LexmlWsConfig.appConfig.getConfig("tasks")
     
  def calcDigest(data: Array[Byte]): String = {
    val alg = config.getString("digest.algorithm")
    val md = MessageDigest.getInstance(alg)
    val hash = md.digest(data)
    "{" + alg + "}" + new Base64().encodeAsString(hash)
  }

  final case class SrcToXhtmlResult(res: List[Node], digest: String)

  def srcToXhtmlTask(texto: Array[Byte], mime: MimeEntrada): List[Node] =
    pipeline(texto, mime.value, new DOCXConverter(new AbiwordConverter)).getOrElse(throw ParseException(FalhaConversaoXHTML(mime.value)))

  def buildMetadado(metadado: RMetadado, digest: String): Metadado = {
    if (!DocumentProfileRegister.autoridades.contains(metadado.autoridade)) {
      throw ParseException(AutoridadeInvalida(metadado.autoridade))
    }    
    val profile = DocumentProfileRegister.getProfile(metadado.autoridade,metadado.tipoNorma, Some("br"))
    				.getOrElse(throw ParseException(TipoNormaInvalido(metadado.tipoNorma)))
    
    val id = metadado.descritorEvento.map(de =>
      Id.fromUrnFrag(de).getOrElse(throw ParseException(DescritorEventoInvalido(de))))

    Metadado(profile, None, None, None, id, Some(digest.getBytes("utf-8")))
  }
  
  def parse(md : Metadado, html : List[Node], opcoes : Option[OpcoesRequisicao]) : (Option[(ProjetoLei,NodeSeq)],List[ParseProblem]) = {
    import _root_.br.gov.lexml.parser.pl.validation.Validation
    val blocks = Block fromNodes html
    val baseProfile = md.profile
    
    val profile : DocumentProfile = opcoes.flatMap(_.profile) match {
      case Some(p) => DocumentProfileOverride(baseProfile,
	    			overrideRegexLocalData = p.regexLocalData.map(_.toList.map(new Regex(_))),
	    			overrideRegexJustificativa = p.regexJustificativa.map(_.toList.map(new Regex(_))),
						overrideRegexAnexos = p.regexAnexos.map(_.toList.map(new Regex(_))),
						overrideRegexLegislacaoCitada = p.regexLegislacaoCitada.map(_.toList.map(new Regex(_))),
						overrideRegexAssinatura = p.regexAssinatura.map(_.toList.map(new Regex(_))),
						overrideRegexEpigrafe = p.regexEpigrafe.map(_.toList.map(new Regex(_))),
						overrideRegexPosEpigrafe = p.regexPosEpigrafe.map(_.toList.map(new Regex(_))),
     				overrideEpigrafeObrigatoria = p.epigrafeObrigatoria,
     				overridePreEpigrafePermitida = p.preEpigrafePermitida,
     				overrideRegexPreambulo = p.regexPreambulo.map(_.toList.map(new Regex(_))),
						overrideUrnFragTipoNorma = p.urnFragTipoNorma,
						overrideEpigrafeHead = p.epigrafeHead,
						overrideEpigrafeTail = p.epigrafeTail,
						overrideUrnFragAutoridade = p.urnFragAutoridade,
						overrideAutoridadeEpigrafe = p.autoridadeEpigrafe,
						overrideEmentaAusente = p.ementaAusente)
      case _ => baseProfile
    }
    val (mpl1, falhas) = new ProjetoLeiParser(profile).fromBlocks(md, blocks)
    val res = for { pl <- mpl1 ; pl2 = pl.remakeEpigrafe } yield (pl2,LexmlRenderer.render(pl2))  
    val falhasXML = if (falhas.nonEmpty) { List() } else {
    	res.map({case (_,xml) => new Validation().validaComSchema(xml).toList}).getOrElse(List())
    }

    (res, falhas ++ falhasXML)    
  }

  def xmlToByteArray(xml : NodeSeq) : Array[Byte] = xml.toString.getBytes("utf-8")
  
  def makeLexMLZip(pl : ProjetoLei, xml : Array[Byte]) : Array[Byte] = {	  
      val bos = new java.io.ByteArrayOutputStream()
      val zos = new java.util.zip.ZipOutputStream(bos)
      zos.setLevel(9)
      zos.setComment(pl.epigrafe.asInstanceOf[Paragraph].text)
      zos.putNextEntry(new java.util.zip.ZipEntry(config.getString("lexml-zip.proposicao-file-name")))
      zos.write(pl.metadado.toXMLmetadadoEditor(pl).toString.getBytes("utf-8"))
      zos.closeEntry()
      zos.putNextEntry(new java.util.zip.ZipEntry(config.getString("lexml-zip.texto-file-name")))      
      zos.write(xml)
      zos.closeEntry()
      zos.close()
      bos.toByteArray    
  }  
  
  def renderPDF(xml : Array[Byte]) : Array[Byte] = {
        val os = new java.io.ByteArrayOutputStream()
        val pdfRenderer = new RendererPDF()
        val pdfConfig = new java.util.HashMap[String, String]()
        pdfConfig.put(PDFConfigs.METADATA_AUTHOR, config.getString("pdf-renderer.author-name"))
        pdfConfig.put(PDFConfigs.FONT_SIZE,config.getString("pdf-renderer.font-size"))
        pdfConfig.put(PDFConfigs.DOCUMENT_MARGIN_RIGHT,config.getString("pdf-renderer.document-margin-right"))
        val is = new java.io.ByteArrayInputStream(xml)
        pdfRenderer.render(is, os, pdfConfig)
        try { os.close() } catch { case _ : Exception => }
        os.toByteArray        
  }
  
  def renderDOCX(xml : Array[Byte]) : Array[Byte] = {
    import br.gov.lexml.renderer.docx._
    val cfg = LexmlToDocxConfig()
    val renderer = new LexmlToDocx(cfg)
    renderer.convert(xml)
  }
  
  def renderDOCXDIFF(xml : Array[Byte]) : Array[Byte] = {
    import br.gov.lexml.renderer.docx._
    
    val cfg = LexmlToDocxConfig().copy(
        constants = Constants.default.copy(skipHyperlink = true))
    
    val renderer = new LexmlToDocx(cfg)
    renderer.convert(xml)
  }
      
  def docToPDF(texto: Array[Byte], mime: MimeEntrada): Array[Byte] = MimeExtensionRegistry.mimeToExtension(mime.value).map((extension : String) => {
    val srcFile = File.createTempFile("lexml-src-render", "." + extension)
    val pdfFile = new File(srcFile.getCanonicalPath.replaceFirst(extension + "$", "pdf"))
    try {
      //val srcPath = srcFile.getCanonicalPath
      FileUtils.writeByteArrayToFile(srcFile, texto)
      val abiwordPath = config.getString("tools.abiword-path")
      val cmd: Array[String] = Array(
        abiwordPath, "--to=pdf", srcFile.getName)
      val p = Runtime.getRuntime.exec(cmd, Array[String](), srcFile.getParentFile)
      p.waitFor
      if(!pdfFile.exists() || !pdfFile.isFile) {
        throw ParseException()
      }
      FileUtils.readFileToByteArray(pdfFile)
    } finally {
        srcFile.delete
        pdfFile.delete
    }
  }).getOrElse(throw new RuntimeException("Tipo MIME de entrada não suportado: " + mime))
  
  def normalizeUrnForSorting(urn : String): String = {
    urn.split("_").toIndexedSeq.map { comp =>
      val compl = comp.split("(?<=[a-z])(?=\\d)").toList
      val name = compl.head
      val nums = compl.tail.headOption.map(_.split("-").toList.map { 
        	case "cpt" => "0000" 
        	case "1u" => "0001" 
        	case x => try { "%04d".format(x.toInt) } catch { case _ : Exception => x } 
        }).mkString("-")
      name + ":" + nums
      
    }.mkString("_")
  }
  
  val reAno: Regex = """(urn:lex:[^:]*:[^:]*:[^:]*:)(\d\d\d\d)-\d\d-\d\d(.*)""".r
  
  def normalizeAno(urn : String): String =
    	reAno.replaceSomeIn(urn, m => Some(m.group(1) + m.group(2) + m.group(3)))
  
  def buildLegislacaoCitada(xhtmlSrc : Seq[Node], urnContexto : String) : Remissoes = {
    val (urns,_) = Linker.findLinks(urnContexto,Text((NodeSeq fromSeq xhtmlSrc).text))
    val urnFrags = urns map ( urn => {
      val p = urn.indexOf('!')
      if(p>=0) {
        (urn.substring(0,p),Set(urn.substring(p+1)))
      } else {
        (urn,Set[String]())
      }
    })    
    val urnMap = urnFrags.groupBy(x => normalizeAno(x._1)).view.mapValues(l => l.map(_._2).foldLeft(Set[String]())(_ union _)).toMap
    val docs : Vector[RemissaoDocumento] = urnMap.toVector.sortBy(_._1).map {
      case (urnDoc,fragSet) =>
        val frags = fragSet.toVector.map(x => (normalizeUrnForSorting(x),x)).sortBy(_._1).map(_._2).map { frag =>
          val urnFrag = urnDoc + "!" + frag
          RemissaoFragmento(
            urn = java.net.URI.create(urnFrag),
            display = buidDisplayDocumento(frag),
            href = buildUrlLexml(urnFrag),
            linkType = XT_Simple)
        }
        RemissaoDocumento(
          urn = java.net.URI.create(urnDoc),
          display = buidDisplayDocumento(urnDoc),
          href = buildUrlLexml(urnDoc),
          linkType = XT_Simple,
          fragmentos = frags)
    }
    Remissoes(documentos = docs )
  }
    
  def makeRemissoes(xhtml : Seq[Node], urnContexto : String) : Array[Byte] = {
    val remissoes = buildLegislacaoCitada(xhtml,urnContexto)
    remissoes.asXML.toString.getBytes("utf-8")
  }
  
  val lexmlUrlFormatString: String = config.getString("tools.lexml-site-urn-format")
  
  def buildUrlLexml(urnDoc : String) : java.net.URI = {
    new java.net.URI(lexmlUrlFormatString.format(urnDoc))
  }
  
  val tagSoupParserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  
  def buidDisplayDocumento(urnDoc : String): String = {
    val parser = tagSoupParserFactory.newSAXParser()
    try {
      val source = new org.xml.sax.InputSource(lexmlUrlFormatString.format(urnDoc))
      source.setEncoding("UTF-8")
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      val doc = adapter.loadXML(source, parser)
      val t = (doc \ "head" \ "title").text
      val i = t.indexOf("::")
      val res = (if (i >= 0) {
        t.substring(0, i)
      } else {
        t
      }).trim
      if (res.isEmpty) {
        urnDoc
      } else {
        res
      }
    } catch {
      case ex : Exception =>
        logger.warn(s"Erro ao buscar documento no site lexml. urnDoc = $urnDoc",ex)
        urnDoc
    }
  }
  
  def buildDisplayFragmento(frag : String): String = FragmentFormatter.format(frag)

  def renderEPUB(xml : Array[Byte]) : Array[Byte] = {
    val renderer = XhtmlRenderer.makeRenderer()
    val xml1 = scala.xml.XML.load(new ByteArrayInputStream(xml))
    val epub = renderer.render(xml1)
    val os = new ByteArrayOutputStream
    epub.writeZip(os)
    os.close()    
    os.toByteArray
  }
      
  def buildDiff(src : Array[Byte], srcMime : String, target : Array[Byte], targetMime : String) :
	  Option[(Array[Byte],Option[Int])] = 
      DiffTask.diffTask.buildDiff(src,srcMime,target,targetMime)    
    
}

