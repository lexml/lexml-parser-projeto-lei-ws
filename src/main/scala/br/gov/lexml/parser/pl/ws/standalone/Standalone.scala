package br.gov.lexml.parser.pl.ws.standalone

import java.io.File
import java.io.FileFilter
import java.net.URI
import org.apache.commons.io.filefilter.SuffixFileFilter
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOCase
import grizzled.slf4j.Logging
import akka.actor.Actor._
import br.gov.lexml.parser.pl.ws.data.scalaxb.DOCUMENTO_ORIGINAL
import br.gov.lexml.parser.pl.ws.data.scalaxb.PDF_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.ParserRequisicao
import br.gov.lexml.parser.pl.ws.data.scalaxb.ScalaxbTipoTextoEmbutidoFormat
import br.gov.lexml.parser.pl.ws.data.scalaxb.TextrtfValue
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoMetadado
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTexto
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTextoEmbutido
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTipoDeSaida
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTiposDeSaidas
import br.gov.lexml.parser.pl.ws.data.scalaxb.XML_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.XHTML_INTERMEDIARIO
import br.gov.lexml.parser.pl.ws.data.scalaxb.XML_REMISSOES
import br.gov.lexml.parser.pl.ws.resources.proc.RequestContext
import br.gov.lexml.parser.pl.ws.Initializer
import br.gov.lexml.parser.pl.ws.resources.proc.RequestProcessor
import org.clapper.argot._
import br.gov.lexml.parser.pl.ws.ServiceParams
import org.apache.commons.io.filefilter.FileFilterUtils
import org.apache.commons.io.filefilter.IOFileFilter
import org.apache.commons.io.filefilter.AbstractFileFilter
import scala.util.matching.Regex
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoMimeEntrada
import br.gov.lexml.parser.pl.ws.data.scalaxb.EXTERNO
import br.gov.lexml.parser.pl.ws.data.scalaxb.EMBUTIDO
import br.gov.lexml.parser.pl.ws.Mime
import br.gov.lexml.parser.pl.profile.DocumentProfileRegister
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoSaida
import br.gov.lexml.parser.pl.ws.data.scalaxb.PDF_DIFF
import br.gov.lexml.parser.pl.ws.data.scalaxb.RTF_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.OpcoesRequisicao
import br.gov.lexml.parser.pl.ws.data.scalaxb.ParserProfile

object Standalone extends Logging {

  val fnameRe = """([a-z.]+)_(\d+)__(\d\d\d\d)_(\d\d\d\d-\d+-\d+)_(.*)"""r
  val fnameRe2 = """([a-z]+)(\d+)(\d\d\d\d)"""r

  def getBasicInfoFromFileName(nome: String) = nome.toLowerCase match {
    case fnameRe(tipo, num, ano, dataEvento, evento) ⇒
      Some((tipo, num, ano, dataEvento, evento))
    case fnameRe2(tipo,num,ano) => Some((tipo,num,ano,"data.evento","leitura"))
    case _ ⇒ {println("nome não reconhecido: " + nome) ; None }
  }

   def buildRequisicao(localidade : String, autoridade: String, f: File, nome: String, extension: String): Option[ParserRequisicao] = {    
    for {
      (tipo, num, ano, dataEvento, evento) ← getBasicInfoFromFileName(nome)
      _ = println("tipo = " + tipo)
      profile <- (DocumentProfileRegister.getProfile(autoridade,tipo) 
          orElse DocumentProfileRegister.profileByAutoridadeSigla(autoridade,tipo))      
      _ = println("profile =  " + profile)
      tipoMime ← Mime.ext2mime.get(extension).map((s: Set[String]) ⇒ TipoMimeEntrada.fromString(s.head))
      _ = println("tipoMime = " + tipoMime)
    } yield {
      val metadado = TipoMetadado(localidade,profile.urnFragAutoridade,profile.urnFragTipoNorma,
        Some("%04d;%d@data.evento;leitura;2011-11-01t00.00" format (ano.toInt, num.toInt)))
      val fileData = FileUtils.readFileToByteArray(f)
      val tipoTextoOption = scalaxb.DataRecord(None, None, TipoTextoEmbutido(scalaxb.Base64Binary(fileData: _*)))
      val texto = TipoTexto(tipoMime, tipoTextoOption)
      val tsaidas = Seq[TipoSaida](DOCUMENTO_ORIGINAL, XHTML_INTERMEDIARIO, PDF_DERIVADO,XML_REMISSOES,XML_DERIVADO,PDF_DIFF, RTF_DERIVADO)
      val ttsaidas = tsaidas.map(TipoTipoDeSaida(_,EXTERNO)) 
      val saidas = TipoTiposDeSaidas(ttsaidas: _*)
      ParserRequisicao(metadado, texto, saidas)
    }
    
    
  }

  def buildRequestContext(destDir: File, localidade : String, autoridade: String): ((File, String, String)) ⇒ Option[RequestContext] = {
    case (f, nome, extension) ⇒ {      
      buildRequisicao(localidade,autoridade, f, nome, extension) map { req ⇒        
        val uniqueId = f.getName.substring(0, f.getName.length - extension.length() - 1) + "_" + extension
        val resultPath = new File(destDir, uniqueId)
        def resultBuilder(s: String*): URI = new URI(f.toURI.toURL.toExternalForm() + "/" + s.mkString("/"))
        resultPath.mkdirs()
        val waitFile = new File(resultPath, "waitFile")
        val dataHoraProcessamento = javax.xml.datatype.DatatypeFactory.newInstance.newXMLGregorianCalendar(new java.util.GregorianCalendar())
        RequestContext(resultBuilder, uniqueId, req,
          resultPath, waitFile, dataHoraProcessamento, None, Some(f.getName))
      }
    }
  }

  def listFiles(extensoes: Set[String]): File ⇒ List[(File, String, String)] = {
    val re = ("^(.*)\\.((?i)" + extensoes.mkString("|") + ")$").r
    val re2 = "^(.*)\\.([^.]*)$".r
    def collectFiles(r: Regex): PartialFunction[(File, String), (File, String, String)] = {
      case (f, r(nome, ext)) ⇒ (f, nome, ext.toLowerCase)
    }
    def doListFiles(f: File): List[(File, String, String)] = {
      if (f.isDirectory) {
        import FileFilterUtils._
        val subDirs = f.listFiles(directoryFileFilter.asInstanceOf[FileFilter]).toList
        val files = f.listFiles(fileFileFilter.asInstanceOf[FileFilter]).toList.map(f ⇒ (f, f.getName)).
          collect(collectFiles(re))
        val subFiles = subDirs.flatMap(doListFiles)
        files ++ subFiles
      } else {
        List((f, f.getName)).collect(collectFiles(re2))
      }
    }
    doListFiles
  }

  def main(args: Array[String]): Unit = {

    println("main: starting")
    Initializer.start()

    import scala.compat.Platform.EOL

    val parser = new ArgotParser("parser-projeto-lei-standalone",
      preUsage = Some("Componentes: " + EOL + ServiceParams.configuracao1.values.mkString("\t", EOL + "\t", EOL)))

    val autoridadeOpt = parser.parameter[String]("autoridade", "Especifica a autoridade: " + DocumentProfileRegister.autoridades.mkString(","), false) {
      (v, opt) ⇒ if (DocumentProfileRegister.autoridades contains v) { v } else { 
            throw new ArgotConversionException("Parâmetro " + opt.name + ":\"" + v + "\" não é uma autoridade válida")          
          }        
    }

    val extensaoOpt = parser.multiOption[String](List("e", "extensao"), "extensão", "Extensão de arquivo a ser considerada no percorrimento de diretórios (default: somente \".rtf\")") {
      (s, opt) ⇒ s
    }

    val inputOpt = parser.multiParameter[File]("entrada", "arquivos ou diretórios a serem processados", false) {
      (s, opt) ⇒
        {
          val file = new File(s)
          if (!file.exists) {
            parser.usage("Arquivo \"" + s + "\" não existe.")
          }
          file
        }
    }

    val destOpt = parser.option[File](List("d", "destino"), "diretório", "Diretório de destino") {
      case (s, opt) ⇒ {
        val file = new File(s)
        if (!file.exists) {
          parser.usage("Arquivo \"" + s + "\" não existe.")
        }
        if (!file.isDirectory) {
          parser.usage("Arquivo \"" + s + "\" não é um diretório.")
        }
        file
      }
    }
    
    val localidadeOpt = parser.option[String](List("localidade"), "localidade", "Localidade no padrao LexML (default: br)") {
      case (s,opt) => s
    }

    try {
      parser.parse(args)
      val autoridade = autoridadeOpt.value.get
      //println("autoridade: " + autoridade)
      val inputs = inputOpt.value.toSet.toList
      println("inputs: " + inputs)
      val extensoes = extensaoOpt.value.isEmpty match {
        case true ⇒ Set("rtf")
        case false ⇒ extensaoOpt.value.toSet
      }
      println("extensoes: " + extensoes)
      val destDir = destOpt.value.getOrElse(new File("."))
      println("destDir: " + destDir)
      val lf = listFiles(extensoes)
      val files = inputs.flatMap(lf)
      val localidade = localidadeOpt.value.getOrElse("br")
      println("files.length: " + files.length)
      val buildReqCtx = buildRequestContext(destDir, localidade, autoridade)
      println("before for loop")
      for { f ← files } {
        println("dealing with: " + f)
        for { req ← buildReqCtx(f) } {          
          Initializer.boot.get.parserServiceRouter ! new RequestProcessor(req)          
        }
      }
    } catch {
      case e: ArgotUsageException ⇒ System.err.println(e.message)
      case ex ⇒ ex.printStackTrace
    }
    while (true) {
      Thread.`yield`();
    }
  }

}