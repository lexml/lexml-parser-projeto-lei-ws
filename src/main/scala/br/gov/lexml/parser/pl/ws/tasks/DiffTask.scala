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
import com.typesafe.config.ConfigFactory
import java.lang.reflect.Method
import grizzled.slf4j.Logging

trait DiffTask {
  def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) :
	  Option[(Array[Byte],Option[Int])]
}

object DiffTask {   
  lazy val diffTask = DelegatedDiffTask().getOrElse(NullDiffTask)    
}

object NullDiffTask extends DiffTask {
  override def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) = None	  
}

class DelegatedDiffTask(clazz : Class[_], m : Method) extends DiffTask {
  import DelegatedDiffTask.logger
  val instance = clazz.newInstance()
    
  override def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) = {
    try {
      m.invoke(instance, from, fromMime, to, toMime).asInstanceOf[Option[(Array[Byte],Option[Int])]]
    } catch {
      case ex : Exception =>
        logger.error(s"Error calling buildDiff method, method=${m}, class=${clazz.getName}")
        None
    }
  }
}

object DelegatedDiffTask extends Logging {
  override val logger = super.logger
  lazy val config = ConfigFactory.load().getConfig("lexml.parser.ws.diff")
  lazy val className = try {
    val cn = Option(config.getString("diff-task-impl-class")).filterNot(_.isEmpty())
    if(cn.isEmpty) {
      logger.info("Diff className was not provided. Diff task will be skipped.")
    }
    cn
  } catch { case _ : Exception => None }
  
  lazy val buildDiffMethod = classOf[DiffTask].getMethods().filter(_.getName() == "buildDiff").head
  lazy val buildDiffMethodParams = buildDiffMethod.getParameterTypes()  
  lazy val clazzMethod = for {
    clsName <- className
    clz <- 
      try {
        Option(
          Thread.currentThread()
            .getContextClassLoader()
            .loadClass(clsName) ) 
      } catch { 
        case _ : ClassNotFoundException =>
          logger.info(s"Diff class not found: ${clsName}")
          None 
      }
    canBeInstantiated = try {
        clz.newInstance()
        true
      } catch {
        case _ : Exception =>
          logger.error(s"Diff class cannot be instantiated: ${clz.getName}")
          false
      } 
    if (canBeInstantiated)
    m <- 
      try {
        Option(clz.getMethod(buildDiffMethod.getName,buildDiffMethodParams : _*))
      } catch {
        case _ : Exception =>
          logger.error(s"buildDiff method not found in Diff class: ${clz.getName}")
          None  
      }   
  } yield ((clz : Class[_],m : Method))    
  
  def apply() = for {
      (clz,m) <- clazzMethod      
    } yield { new DelegatedDiffTask(clz,m) }     
}

