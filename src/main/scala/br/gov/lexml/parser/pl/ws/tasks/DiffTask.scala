package br.gov.lexml.parser.pl.ws.tasks

import com.sun.jersey.api.client.config.DefaultClientConfig
import com.sun.jersey.multipart.FormDataMultiPart
import com.sun.jersey.multipart.FormDataBodyPart
import com.sun.jersey.core.header.ContentDisposition
import com.sun.jersey.api.client.Client
import br.gov.lexml.parser.pl.ws.MimeExtensionRegistry
import javax.ws.rs.core.MediaType
import br.gov.lexml.parser.pl.ws.LexmlWsConfig
import com.typesafe.config.Config
import com.sun.jersey.api.client.ClientResponse
import scala.util.matching.Regex

trait DiffTask {
  def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) :
	  Option[(Array[Byte],Option[Int])]
}

class NullDiffTask extends DiffTask {
  override def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) = None	  
}

class DefaultDiffTask extends DiffTask {
  val config : Config = 
    LexmlWsConfig.config.getConfig("tasks.diff.default-impl") 
  
  val numDiffsRe: Regex = "diffs_(\\d+)"r
  
  def buildDiff(src : Array[Byte], srcMime : String, target : Array[Byte], targetMime : String) :
  	  Option[(Array[Byte],Option[Int])] = {    
    val srcExtension = MimeExtensionRegistry.mimeToExtension(srcMime).map("." + _).getOrElse("")
    val targetExtension = MimeExtensionRegistry.mimeToExtension(targetMime).map("." + _).getOrElse("")
    val srcName = "source" + srcExtension
    val targetName = "target" + targetExtension
    
    val cconfig = new DefaultClientConfig()

    val c = Client.create(cconfig)
    c.setFollowRedirects(true)    
    //set timeout
    c.setConnectTimeout(10*1000)
    c.setReadTimeout(30*1000)
    //
    val wr = c.resource(config.getString("office-automation-url"))
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
    if(resp.getStatus == 200) {
      val cd = new ContentDisposition(resp.getHeaders.get("Content-Disposition").get(0))
      val fileName = cd.getFileName
      val m = numDiffsRe.findFirstMatchIn(fileName)            
      val numDiffs = m.map(_.group(1).toInt)
      val data = resp.getEntity(classOf[Array[Byte]])
      Some(data,numDiffs)
    } else {
      None
    }              
  }
}