package br.gov.lexml.parser.pl.ws
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.InputStreamReader
import eu.medsea.mimeutil.MimeUtil2
import eu.medsea.mimeutil.MimeUtil
import eu.medsea.mimeutil.detector.MimeDetector
import grizzled.slf4j.Logging

object Mime extends Logging {
  
  /*lazy val ext2mime : Map[String,Set[String]]= {
      import scala.collection.JavaConversions._
      val lines = IOUtils.readLines(new InputStreamReader(classOf[MimeUtil2].getResourceAsStream("mime-types.properties"))).toList      
      lines map (_.split("[=,]").toList) collect {
        case ext :: mimetypes => (ext -> mimetypes.toSet)
      } toMap            
    }*/
  val ext2mime: Map[String, Set[String]] = Map(
    "doc" -> "application/msword",
    "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "txt" -> "text/plain",
    "html" -> "text/html",
    "odt" -> "application/vnd.oasis.opendocument.text",
    "rtf" -> "text/rtf",
    "pdf" -> "application/pdf",
    "xml" -> "text/xml",
    "zip" -> "application/zip",
    "xsl" -> "application/xslt+xml").map { case (k, v) ⇒ (k, Set(v)) }
  lazy val mime2ext: Map[String, Set[String]] = {
    val l = for { (k, s) ← ext2mime; t ← s } yield (t, k)
    l.foldLeft(Map[String, Set[String]]()) {
      case (m, (t, k)) ⇒ m + (t -> (m.getOrElse(t, Set[String]()) + k))
    }
  }

  def mimeToExtension(mimeType: String): Option[String] = mime2ext.get(mimeType).map(_.head)

  val detectors = List[Class[_]](
    classOf[eu.medsea.mimeutil.detector.MagicMimeMimeDetector],
    classOf[eu.medsea.mimeutil.detector.OpendesktopMimeDetector])
  def init() = synchronized {
    detectors.foreach(c => try { 
    	MimeUtil.registerMimeDetector(c.getName)
    } catch {
      case ex : Exception =>
        logger.warn("Error registering mime detector",ex)
    }
    )   
  }

}