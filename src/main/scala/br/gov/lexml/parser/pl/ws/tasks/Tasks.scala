package br.gov.lexml.parser.pl.ws.tasks

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.security.MessageDigest
import javax.ws.rs.core.MediaType

import br.gov.lexml.parser.pl.{ProjetoLei, ProjetoLeiParser}
import br.gov.lexml.parser.pl.block.{Block, Paragraph}
import br.gov.lexml.parser.pl.errors.{ParseException, ParseProblem}
import br.gov.lexml.parser.pl.linker.Linker
import br.gov.lexml.parser.pl.metadado.{Id, Metadado}
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.profile.{DocumentProfile, DocumentProfileOverride, DocumentProfileRegister}
import br.gov.lexml.parser.pl.ws.{LexmlWsConfig, MimeExtensionRegistry}
import br.gov.lexml.parser.pl.ws.data.scalaxb.{OpcoesRequisicao, TipoMetadado, TipoMimeEntrada}
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.{ScalaxbTRemissoesFormat, Simple, TRemissaoDocumento, TRemissaoFragmento, TRemissoes, defaultScope}
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor.{defaultConverter, pipeline}
import br.gov.lexml.renderer.pdf.{PDFConfigs, RendererPDF}
import br.gov.lexml.renderer.rtf.{RTFBuilder, RendererRTFContext}
import br.gov.lexml.renderer.strategies.XhtmlRenderer
import com.sun.jersey.api.client.{Client, ClientResponse}
import com.sun.jersey.api.client.config.DefaultClientConfig
import com.sun.jersey.core.header.ContentDisposition
import com.sun.jersey.multipart.{FormDataBodyPart, FormDataMultiPart}
import com.typesafe.config.Config
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.FileUtils
import org.dom4j.io.SAXReader

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.xml.{Elem, Node, NodeSeq, Text}
import grizzled.slf4j.Logging

object Tasks extends Logging {
  
  val config: Config = LexmlWsConfig.config.getConfig("tasks")
     
  def calcDigest(data: Array[Byte]): String = {
    val alg = config.getString("digest.algorithm")
    val md = MessageDigest.getInstance(alg)
    val hash = md.digest(data)
    "{" + alg + "}" + new Base64().encodeAsString(hash)
  }

  final case class SrcToXhtmlResult(res: List[Node], digest: String)

  def srcToXhtmlTask(texto: Array[Byte], mime: TipoMimeEntrada): List[Node] =
    pipeline(texto, mime.toString, defaultConverter).getOrElse(throw ParseException(FalhaConversaoXHTML(mime.toString)))

  def buildMetadado(metadado: TipoMetadado, digest: String): Metadado = {
    if (!DocumentProfileRegister.autoridades.contains(metadado.autoridade)) {
      throw ParseException(AutoridadeInvalida(metadado.autoridade))
    }    
    val profile = DocumentProfileRegister.getProfile(metadado.autoridade,metadado.tipoNorma, Some("br"))
    				.getOrElse(throw ParseException(TipoNormaInvalido(metadado.tipoNorma)))
    
    val id = metadado.descritorEvento.map(de ⇒
      Id.fromUrnFrag(de).getOrElse(throw ParseException(DescritorEventoInvalido(de))))

    Metadado(profile, None, None, None, id, Some(digest.getBytes("utf-8")))
  }
  
  def parse(md : Metadado, html : List[Node], opcoes : Option[OpcoesRequisicao]) : (Option[(ProjetoLei,NodeSeq)],List[ParseProblem]) = {
    import _root_.br.gov.lexml.parser.pl.validation.Validation
    val blocks = Block fromNodes html
    val baseProfile = md.profile
    
    val profile : DocumentProfile = opcoes.flatMap(_.profile) match {
      case Some(p) => DocumentProfileOverride(baseProfile,
	    				overrideRegexLocalData = p.regexLocalData.map(_.regex.toList.map(new Regex(_))),
	    				overrideRegexJustificativa = p.regexJustificativa.map(_.regex.toList.map(new Regex(_))),
						overrideRegexAnexos = p.regexAnexos.map(_.regex.toList.map(new Regex(_))),
						overrideRegexLegislacaoCitada = p.regexLegislacaoCitada.map(_.regex.toList.map(new Regex(_))),
						overrideRegexAssinatura = p.regexAssinatura.map(_.regex.toList.map(new Regex(_))),
						overrideRegexEpigrafe = p.regexEpigrafe.map(_.regex.toList.map(new Regex(_))),
						overrideRegexPosEpigrafe = p.regexPosEpigrafe.map(_.regex.toList.map(new Regex(_))),        				
        				overrideEpigrafeObrigatoria = p.epigrafeObrigatoria,
        				overridePreEpigrafePermitida = p.preEpigrafePermitida,
        				overrideRegexPreambulo = p.regexPreambulo.map(_.regex.toList.map(new Regex(_))),
						overrideUrnFragTipoNorma = p.urnFragTipoNorma,
						overrideEpigrafeHead = p.epigrafeHead,
						overrideEpigrafeTail = p.epigrafeTail,
						overrideUrnFragAutoridade = p.urnFragAutoridade,
						overrideAutoridadeEpigrafe = p.autoridadeEpigrafe.map(_.autoridadeEpigrafeValue),
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
  
  def renderPDF(xml : Array[Byte], md : Metadado) : Array[Byte] = {        
        val os = new java.io.ByteArrayOutputStream()
        val pdfRenderer = new RendererPDF()
        val pdfConfig = new java.util.HashMap[String, String]()
        pdfConfig.put(PDFConfigs.METADATA_AUTHOR, config.getString("pdf-renderer.author-name"))
        pdfConfig.put(PDFConfigs.FONT_SIZE,config.getString("pdf-renderer.font-size"))
        pdfConfig.put(PDFConfigs.DOCUMENT_MARGIN_RIGHT,config.getString("pdf-renderer.document-margin-right"))
        val is = new java.io.ByteArrayInputStream(xml)
        pdfRenderer.render(is, os, pdfConfig)
        try { os.close() } catch { case _ : Exception ⇒ }
        os.toByteArray        
  }
  
  def renderRTF(xml : Array[Byte], md : Metadado) : Array[Byte] = {
        val os = new java.io.ByteArrayOutputStream()        
        val pdfConfig = new java.util.HashMap[String, String]()
        pdfConfig.put(PDFConfigs.METADATA_AUTHOR, config.getString("rtf-renderer.author-name"))
        pdfConfig.put(PDFConfigs.FONT_SIZE,config.getString("rtf-renderer.font-size"))
        pdfConfig.put(PDFConfigs.DOCUMENT_MARGIN_RIGHT,config.getString("rtf-renderer.document-margin-right"))
        val is = new java.io.ByteArrayInputStream(xml)        
        val ctx = new RendererRTFContext(md.profile.urnFragAutoridade,md.profile.urnFragTipoNorma)
        ctx.addConfig(pdfConfig)
        val reader = new SAXReader()
        reader.setEncoding("utf-8")
        val document = reader.read(is)
        val root = document.getRootElement
        ctx.setOutputStream(os)
        new RTFBuilder(ctx, root).build()
        try { os.close() } catch { case _ : Exception => }        
        os.toByteArray
  }
  
  def renderDOCX(xml : Array[Byte]) : Array[Byte] = {
    import br.gov.lexml.renderer.docx._
    val cfg = LexmlToDocxConfig()
    val renderer = new LexmlToDocx(cfg)
    renderer.convert(xml)
  }
      
  def docToPDF(texto: Array[Byte], mime: TipoMimeEntrada): Array[Byte] = MimeExtensionRegistry.mimeToExtension(mime.toString).map((extension : String) => {
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
  }).getOrElse(throw new RuntimeException("Tipo MIME de entrada não suportado: " + mime.toString))
  
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
  
  def buildLegislacaoCitada(xhtmlSrc : Seq[Node]) : TRemissoes = {
    val (urns,_) = Linker.findLinks(Text((NodeSeq fromSeq xhtmlSrc).text))
    val urnFrags = urns map ( urn => {
      val p = urn.indexOf('!')
      if(p>=0) {
        (urn.substring(0,p),Set(urn.substring(p+1)))
      } else {
        (urn,Set[String]())
      }
    })    
    val urnMap = urnFrags.groupBy(x => normalizeAno(x._1)).mapValues(l => l.map(_._2).foldLeft(Set[String]())(_ union _))
    val docs = urnMap.toIndexedSeq.sortBy(_._1).map {
      case (urnDoc,fragSet) =>
        val frags = fragSet.toIndexedSeq.map(x => (normalizeUrnForSorting(x),x)).sortBy(_._1).map(_._2).map { frag =>
          val urnFrag = urnDoc + "!" + frag
          TRemissaoFragmento(new java.net.URI(urnFrag),buildDisplayFragmento(frag),buildUrlLexml(urnFrag),Simple)
        }
        TRemissaoDocumento(frags,new java.net.URI(urnDoc),
            buidDisplayDocumento(urnDoc),
            buildUrlLexml(urnDoc), Simple)

    }
    TRemissoes(docs : _*)    
  }
    
  def makeRemissoes(xhtml : Seq[Node]) : Array[Byte] = {
    val remissoes = buildLegislacaoCitada(xhtml)
    val psXml = scalaxb_1_1.toXML[TRemissoes](remissoes, None, Some("Remissoes"), defaultScope).
                    head.asInstanceOf[Elem]
    val psXml1 = psXml.copy(prefix = "tns")
    psXml1.toString.getBytes("utf-8")    
  }
  
  val lexmlUrlFormatString: String = config.getString("tools.lexml-site-urn-format")
  
  def buildUrlLexml(urnDoc : String) : java.net.URI = {
    new java.net.URI(lexmlUrlFormatString.format(urnDoc))
  }
  
  val tagSoupParserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  
  def buidDisplayDocumento(urnDoc : String): String = {
    val parser = tagSoupParserFactory.newSAXParser()
    val source = new org.xml.sax.InputSource(lexmlUrlFormatString.format(urnDoc))
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    val doc = adapter.loadXML(source, parser)
    val t = (doc \ "head" \ "title").text
    val i = t.indexOf("::") 
    val res = (if( i >= 0) { t.substring(0,i) } else { t }).trim
    if (res.length == 0) { urnDoc } else { res }
  }
  
  def buildDisplayFragmento(frag : String): String = FragmentFormatter.format(frag)

  def renderEPUB(xml : Array[Byte], md : Metadado) : Array[Byte] = {
    val renderer = XhtmlRenderer.makeRenderer()
    val xml1 = scala.xml.XML.load(new ByteArrayInputStream(xml))
    val epub = renderer.render(xml1)
    val os = new ByteArrayOutputStream
    epub.writeZip(os)
    os.close()    
    os.toByteArray
  }
  
  lazy val diffTask : DiffTask = {    
    val cfg = config.getConfig("diff")
    if(cfg.getBoolean("skip")) {
      new NullDiffTask()
    } else {
      Option(cfg.getString("diff-task-impl-class")).
      flatMap(clsName => 
        try {
          val c = Class.forName(clsName).asSubclass(classOf[DiffTask])          
          Some(c.newInstance())          
        } catch {
          case ex : Exception =>
            logger.error(s"Não foi possível instanciar DiffTask: classe '${clsName}'")
            None
        }).getOrElse(new DefaultDiffTask())
    }
  }
  
  def buildDiff(src : Array[Byte], srcMime : String, target : Array[Byte], targetMime : String) :
	  Option[(Array[Byte],Option[Int])] = 
      diffTask.buildDiff(src,srcMime,target,targetMime)    
    
}

