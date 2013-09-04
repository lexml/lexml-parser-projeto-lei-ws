package br.gov.lexml.parser.pl.ws.tasks

import java.io.File
import java.security.MessageDigest
import scala.collection.JavaConversions.asScalaSet
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Text
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.FileUtils
import org.dom4j.io.SAXReader
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.errors.ParseException
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.linker.Linker
import br.gov.lexml.parser.pl.metadado.Id
import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.profile.DocumentProfileRegister
import br.gov.lexml.parser.pl.profile.DocumentProfileOverride
import br.gov.lexml.parser.pl.validation.Validation
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoMetadado
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoMimeEntrada
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.ScalaxbTRemissoesFormat
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.Simple
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.TRemissaoDocumento
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.TRemissaoFragmento
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.TRemissoes
import br.gov.lexml.parser.pl.ws.remissoes.scalaxb.defaultScope
import br.gov.lexml.parser.pl.ws.Mime
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor.defaultConverter
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor.pipeline
import br.gov.lexml.parser.pl.ProjetoLei
import br.gov.lexml.parser.pl.ProjetoLeiParser
import br.gov.lexml.renderer.pdf.PDFConfigs
import br.gov.lexml.renderer.pdf.RendererPDF
import br.gov.lexml.renderer.rtf.RTFBuilder
import br.gov.lexml.renderer.rtf.RendererRTFContext
import br.gov.lexml.renderer.rtf.RendererRTF
import br.gov.lexml.renderer.strategies.XhtmlRenderer
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import com.sun.jersey.api.representation.Form
import com.sun.jersey.api.client.Client
import com.sun.jersey.api.client.config.DefaultClientConfig
import com.sun.jersey.multipart.FormDataBodyPart
import com.sun.jersey.core.header.FormDataContentDisposition
import javax.ws.rs.core.MediaType
import com.sun.jersey.multipart.FormDataMultiPart
import com.sun.jersey.api.client.ClientResponse
import com.sun.jersey.core.header.ContentDisposition
import br.gov.lexml.parser.pl.ws.data.scalaxb.OpcoesRequisicao
import scala.util.matching.Regex
import br.gov.lexml.parser.pl.profile.DocumentProfile
import scala.util.matching.Regex

object Tasks {

  def calcDigest(data: Array[Byte]): String = {
    val alg = "sha-1"
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
    val profile = DocumentProfileRegister.getProfile(metadado.autoridade,metadado.tipoNorma)
    				.getOrElse(throw new ParseException(TipoNormaInvalido(metadado.tipoNorma)))
    
    val id = metadado.descritorEvento.map(de ⇒
      Id.fromUrnFrag(de).getOrElse(throw ParseException(DescritorEventoInvalido(de))))

    Metadado(profile, id, Some(digest.getBytes("utf-8")))
  }
  
  def parse(md : Metadado, html : List[Node], opcoes : Option[OpcoesRequisicao]) : (Option[(ProjetoLei,NodeSeq)],List[ParseProblem]) = {
    import _root_.br.gov.lexml.parser.pl.validation.Validation    
    
    val blocks = Block fromNodes html
    val baseProfile = md.profile
    
    val profile : DocumentProfile = opcoes.flatMap(_.profile) match {
      case Some(p) => new DocumentProfileOverride(baseProfile,
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
						overrideAutoridadeEpigrafe = p.autoridadeEpigrafe.map(_.autoridadeEpigrafeValue))
      case _ => baseProfile
    }
    val (mpl1, falhas) = new ProjetoLeiParser(profile).fromBlocks(md, blocks)
    val res = for { pl <- mpl1 ; pl2 = pl.remakeEpigrafe } yield (pl2,LexmlRenderer.render(pl2))  
    val falhasXML = if (!falhas.isEmpty) { List() } else {
    	res.map({case (_,xml) => Validation.validaComSchema(xml).toList}).getOrElse(List())
    }

    (res, falhas ++ falhasXML)    
  }

  def xmlToByteArray(xml : NodeSeq) : Array[Byte] = xml.toString.getBytes("utf-8")
  
  def makeLexMLZip(pl : ProjetoLei, xml : Array[Byte]) : Array[Byte] = {	  
      val bos = new java.io.ByteArrayOutputStream()
      val zos = new java.util.zip.ZipOutputStream(bos)
      zos.setLevel(9)
      zos.setComment(pl.epigrafe.asInstanceOf[Paragraph].text)
      zos.putNextEntry(new java.util.zip.ZipEntry("proposicao.xml"))
      zos.write(pl.metadado.toXMLmetadadoEditor(pl).toString.getBytes("utf-8"))
      zos.closeEntry()
      zos.putNextEntry(new java.util.zip.ZipEntry("texto.xml"))      
      zos.write(xml)
      zos.closeEntry()
      zos.close()
      bos.toByteArray    
  }  
  
  def renderPDF(xml : Array[Byte], md : Metadado) : Array[Byte] = {        
        val os = new java.io.ByteArrayOutputStream()
        val pdfRenderer = new RendererPDF()
        val config = new java.util.HashMap[String, String]()
        config.put(PDFConfigs.METADATA_AUTHOR, "LexML Parser")        
        config.put(PDFConfigs.FONT_SIZE,"14")
        config.put(PDFConfigs.DOCUMENT_MARGIN_RIGHT,"2")        
        val is = new java.io.ByteArrayInputStream(xml)
        pdfRenderer.render(is, os, config)
        try { os.close } catch { case _ ⇒ }
        os.toByteArray        
  }
  
  def renderRTF(xml : Array[Byte], md : Metadado) : Array[Byte] = {
        val os = new java.io.ByteArrayOutputStream()        
        val config = new java.util.HashMap[String, String]()
        config.put(PDFConfigs.METADATA_AUTHOR, "LexML Parser")
        config.put(PDFConfigs.FONT_SIZE,"14")
        config.put(PDFConfigs.DOCUMENT_MARGIN_RIGHT,"2")
        val is = new java.io.ByteArrayInputStream(xml)        
        val ctx = new RendererRTFContext(md.profile.urnFragAutoridade,md.profile.urnFragTipoNorma)
        ctx.addConfig(config);
        val reader = new SAXReader();
        val document = reader.read(is);
        val root = document.getRootElement();
        ctx.setOutputStream(os);                
        new RTFBuilder(ctx, root).build();        
        try { os.close() } catch { case _ => }        
        os.toByteArray
  }
      
  def docToPDF(texto: Array[Byte], mime: TipoMimeEntrada): Array[Byte] = Mime.mimeToExtension(mime.toString).map( (extension : String) => {
    val srcFile = File.createTempFile("lexml-src-render", "." + extension)
    val pdfFile = new File(srcFile.getCanonicalPath.replaceFirst(extension + "$", "pdf"))
    try {
      //val srcPath = srcFile.getCanonicalPath
      FileUtils.writeByteArrayToFile(srcFile, texto)
      val cmd: Array[String] = Array(
        "/usr/bin/abiword", "--to=pdf", srcFile.getName)
      val p = Runtime.getRuntime.exec(cmd, Array[String](), srcFile.getParentFile)
      p.waitFor
      if(!pdfFile.exists() || !pdfFile.isFile()) {
        throw new ParseException()
      }
      FileUtils.readFileToByteArray(pdfFile)
    } finally {
        srcFile.delete
        pdfFile.delete
    }
  }).getOrElse(throw new RuntimeException("Tipo MIME de entrada não suportado: " + mime.toString))
  
  def normalizeUrnForSorting(urn : String) = {
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
  
  val reAno = """(urn:lex:[^:]*:[^:]*:[^:]*:)(\d\d\d\d)-\d\d-\d\d(.*)""".r
  
  def normalizeAno(urn : String) = 
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
      case (urnDoc,fragSet) => {
        val frags = fragSet.toIndexedSeq.map(x => (normalizeUrnForSorting(x),x)).sortBy(_._1).map(_._2).map { frag =>
          val urnFrag = urnDoc + "!" + frag
          TRemissaoFragmento(new java.net.URI(urnFrag),buildDisplayFragmento(frag),buildUrlLexml(urnFrag),Simple)
        }
        TRemissaoDocumento(frags,new java.net.URI(urnDoc),
            buidDisplayDocumento(urnDoc),
            buildUrlLexml(urnDoc), Simple)
      }
    }.toSeq
    TRemissoes(docs : _*)    
  }
  
  def makeRemissoes(xhtml : Seq[Node]) : Array[Byte] = {
    val remissoes = buildLegislacaoCitada(xhtml)
    val psXml = scalaxb.toXML[TRemissoes](remissoes, None, Some("Remissoes"), defaultScope)
    psXml.toString.getBytes("utf-8")    
  }
  
  def buildUrlLexml(urnDoc : String) : java.net.URI = {
    new java.net.URI("http://www.lexml.gov.br/urn/" + urnDoc)
  }
  
  val tagSoupParserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  
  def buidDisplayDocumento(urnDoc : String) = {    
    val parser = tagSoupParserFactory.newSAXParser()
    val source = new org.xml.sax.InputSource("http://www.lexml.gov.br/urn/" + urnDoc)
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    val doc = adapter.loadXML(source, parser)
    val t = (doc \ "head" \ "title").text
    val i = t.indexOf("::") 
    val res = (if( i >= 0) { t.substring(0,i) } else { t }).trim
    if (res.length == 0) { urnDoc } else { res }
  }
  
  def buildDisplayFragmento(frag : String) = FragmentFormatter.format(frag)

  def renderEPUB(xml : Array[Byte], md : Metadado) : Array[Byte] = {
    val renderer = XhtmlRenderer.makeRenderer()
    val xml1 = scala.xml.XML.load(new ByteArrayInputStream(xml))
    val epub = renderer.render(xml1)
    val os = new ByteArrayOutputStream
    epub.writeZip(os)
    os.close()    
    os.toByteArray
  }
  
  val numDiffsRe = "diffs_(\\d+)"r
  def buildDiff(src : Array[Byte], srcMime : String, target : Array[Byte], targetMime : String) :
	  Option[(Array[Byte],Option[Int])] = {
    val srcExtension = Mime.mimeToExtension(srcMime).map("." + _).getOrElse("")
    val targetExtension = Mime.mimeToExtension(targetMime).map("." + _).getOrElse("")
    val srcName = "source" + srcExtension
    val targetName = "target" + targetExtension
    
    val cconfig = new DefaultClientConfig()
    val c = Client.create(cconfig)
    c.setFollowRedirects(true)    
    val wr = c.resource("http://intra1.senado.gov.br/office-automation/upload.aspx")
    import com.sun.jersey.core.header.{FormDataContentDisposition => B}
    
    val srcBodyPart = new FormDataBodyPart(
        B.name("origem").fileName(srcName).build(),src,MediaType.valueOf(srcMime))
    val targetBodyPart = new FormDataBodyPart(
        B.name("revisao").fileName(targetName).build(),target,MediaType.valueOf(targetMime))
    val fdmp = new FormDataMultiPart()
    fdmp.bodyPart(srcBodyPart)
    fdmp.bodyPart(targetBodyPart)
    fdmp.field("operacao","compare") 
    fdmp.field("formatoSaida","pdf")
    val resp = wr.`type`(MediaType.MULTIPART_FORM_DATA_TYPE).post(classOf[ClientResponse],fdmp)
    if(resp.getStatus() == 200) {
      val cd = new ContentDisposition(resp.getHeaders().get("Content-Disposition").get(0))
      val fileName = cd.getFileName()      
      val m = numDiffsRe.findFirstMatchIn(fileName)            
      val numDiffs = m.map(_.group(1).toInt)
      val data = resp.getEntity(classOf[Array[Byte]])
      Some(data,numDiffs)
    } else {
      None
    }          
  }
    
  /*def getMensagemUsuario(prob : ParseProblem) : Option[String] = {
    
  }*/
}

