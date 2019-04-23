package br.gov.lexml.parser.pl.ws.standalone

import java.io.{File, FileFilter}
import java.net.URI

import br.gov.lexml.parser.pl.profile.DocumentProfileRegister
import br.gov.lexml.parser.pl.ws.{Initializer, MimeExtensionRegistry, ServiceParams}
import br.gov.lexml.parser.pl.ws.data.scalaxb._
import br.gov.lexml.parser.pl.ws.resources.proc.{RequestContext, RequestProcessor}
import grizzled.slf4j.Logging
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.FileFilterUtils
import org.clapper.argot._

import scala.language.postfixOps
import scala.util.matching.Regex
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoFormatoSaida

object Standalone extends Logging {

  val fnameRe: Regex = """([a-z.]+)_(\d+)__(\d\d\d\d)_(\d\d\d\d-\d+-\d+)_(.*)"""r
  val fnameRe2: Regex = """([a-z]+)(\d+)(\d\d\d\d)"""r

  def getBasicInfoFromFileName(nome: String): Option[(String, String, String, String, String)] = nome.toLowerCase match {
    case fnameRe(tipo, num, ano, dataEvento, evento) ⇒
      Some((tipo, num, ano, dataEvento, evento))
    case fnameRe2(tipo,num,ano) => Some((tipo,num,ano,"data.evento","leitura"))
    case _ ⇒
      println("nome não reconhecido: " + nome)
      None
  }

   def buildRequisicao(localidade : String, autoridade: String, f: File, nome: String, extension: String): Option[ParserRequisicao] = {    
    for {
      (tipo, num, ano, dataEvento, evento) ← getBasicInfoFromFileName(nome)
      _ = println("tipo = " + tipo)
      profile <- (DocumentProfileRegister.getProfile(autoridade,tipo) 
          orElse DocumentProfileRegister.profileByAutoridadeSigla(autoridade,tipo))      
      _ = println("profile =  " + profile)
      tipoMime ← MimeExtensionRegistry.ext2mime.get(extension).map((s: Set[String]) ⇒ TipoMimeEntrada.fromString(s.head,defaultScope))
      _ = println("tipoMime = " + tipoMime)
    } yield {
      val metadado = TipoMetadado(localidade,profile.urnFragAutoridade,profile.urnFragTipoNorma,
        Some("%04d;%d@data.evento;leitura;2011-11-01t00.00" format (ano.toInt, num.toInt)))
      val fileData = FileUtils.readFileToByteArray(f)
      val tipoTextoOption = scalaxb.DataRecord(None, None, TipoTextoEmbutido(scalaxb.Base64Binary(fileData: _*)))
      val texto = TipoTexto(tipoMime, tipoTextoOption)
      val tsaidas = Seq[TipoSaida](DOCUMENTO_ORIGINAL, XHTML_INTERMEDIARIO, PDF_DERIVADO,XML_REMISSOES,XML_DERIVADO,PDF_DIFF, RTF_DERIVADO)
      val ttsaidas = tsaidas.map(x => TipoTipoDeSaida(Map("@tipo" -> scalaxb.DataRecord(x), "@formato" -> scalaxb.DataRecord(EXTERNO : TipoFormatoSaida)))) 
      val saidas = TipoTiposDeSaidas(ttsaidas)
      ParserRequisicao(metadado, texto, saidas)
    }
    
    
  }

  def buildRequestContext(destDir: File, localidade : String, autoridade: String): ((File, String, String)) ⇒ Option[RequestContext] = {
    case (f, nome, extension) ⇒
      buildRequisicao(localidade,autoridade, f, nome, extension) map { req ⇒        
        val uniqueId = f.getName.substring(0, f.getName.length - extension.length() - 1) + "_" + extension
        val resultPath = new File(destDir, uniqueId)
        def resultBuilder(s: String*): URI = new URI(f.toURI.toURL.toExternalForm + "/" + s.mkString("/"))
        resultPath.mkdirs()
        val waitFile = new File(resultPath, "waitFile")
        val dataHoraProcessamento = javax.xml.datatype.DatatypeFactory.newInstance.newXMLGregorianCalendar(new java.util.GregorianCalendar())
        RequestContext(resultBuilder, uniqueId, req,
          resultPath, waitFile, dataHoraProcessamento, None, Some(f.getName))
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

    val mandatory = false
    val autoridadeOpt = parser.parameter[String]("autoridade", "Especifica a autoridade: " + DocumentProfileRegister.autoridades.mkString(","), mandatory) {
      (v, opt) ⇒ if (DocumentProfileRegister.autoridades contains v) { v } else { 
            throw new ArgotConversionException("Parâmetro " + opt.name + ":\"" + v + "\" não é uma autoridade válida")          
          }        
    }

    val extensaoOpt = parser.multiOption[String](List("e", "extensao"), "extensão", "Extensão de arquivo a ser considerada no percorrimento de diretórios (default: somente \".rtf\")") {
      (s, opt) ⇒ s
    }

    val inputOpt = parser.multiParameter[File]("entrada", "arquivos ou diretórios a serem processados", mandatory) {
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
      case (s, opt) ⇒
        val file = new File(s)
        if (!file.exists) {
          parser.usage("Arquivo \"" + s + "\" não existe.")
        }
        if (!file.isDirectory) {
          parser.usage("Arquivo \"" + s + "\" não é um diretório.")
        }
        file

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
      val extensoes = if (extensaoOpt.value.isEmpty) {
        Set("rtf")
      } else {
        extensaoOpt.value.toSet
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
      case ex : Exception ⇒ ex.printStackTrace()
    }
    while (true) {
      Thread.`yield`()
    }
  }

}