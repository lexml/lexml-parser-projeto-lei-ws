package br.gov.lexml.parser.pl.ws

import javax.servlet.{ServletContextEvent, ServletContextListener}
import java.io.File

import br.gov.lexml.parser.pl.ws.data.scalaxb._
import javax.servlet.ServletContext

import grizzled.slf4j.Logging
import br.gov.lexml.parser.pl.cfg.ParserComponentConfiguration
import br.gov.lexml.parser.pl.cfg.ParserComponentConfiguration.Configuration


class ServiceParams(octx : Option[ServletContext] = None) extends Logging {  
    def option[T](name : String,default : => T)(implicit canBuildFrom : String => Option[T]) = {
      octx match {
        case None => {          
          val v : T = default
          logger.info("no context: option '" + name + "' set to default = " + v)
          v
        }
        case Some(ctx) => {          
          Option(ctx.getInitParameter(name)) match {                       
            case None => {
              val v : T = default
              logger.info("context exists: no parameter for option '" + name + "'. setting to default = " + v)
              v
            }
            case Some(str) => canBuildFrom(str) match {
              case None => {
                val v : T = default
                logger.info("context exists, option '" + name + "' is set to '" + str + "'. can't build value. setting to default = " + v)
                v 
              }
              case Some(v) => {
                logger.info("context exists, option '" + name + "' is set to '" + str + "'. setting to = " + v)
                v
              }
            }
          }         
        }
      }
      //octx.flatMap(ctx => Option(ctx.getInitParameter(name)).flatMap(canBuildFrom)).getOrElse(default)
    }
    
    import scala.language.implicitConversions
        
    implicit def canBuildFile(path : String) : Option[File] = {
      val f = new File(path).getCanonicalFile()
      logger.debug("canBuildFile: path = " + path + ", f = " + f + ", exists = " + f.exists)
      if(f.exists()) { Some(f) } else { None }      
    }
    
    implicit def canBuildInt(num : String) : Option[Int] =
      try { Some(num.toInt) } catch { case _ : Exception => None }      
      
    var parseResultDirectory : File = option("parseResultDirectory",new File("/tmp/parser/result"))
    var incompleteCleaningIntervalMinutes : Int = option("resultDirectoryIncompleteCleaningIntervalMinutes",1)
    var completeCleaningIntervalMinutes : Int = option("resultDirectoryCompleteCleaningIntervalMinutes",3)
    val defaultStaticDir = try {
      new File("src/main/resources").getCanonicalFile
    } catch { case ex : Exception =>
      logger.warn("default static directory not present", ex)
      new File("src/main/resources")
    }
    var staticOverrideDirectory : File = option("staticOverrideDirectory",defaultStaticDir)    
    var diretorioMensagemUsuario : File = option("mensagemUsuarioDirectory",new File("/home/joao/workspace/lexml-parser-projeto-lei-mensagem-usuario/target"))
}

object ServiceParams {
  
  var params : ServiceParams = new ServiceParams() 
  
  lazy val configuracao1: Configuration = ParserComponentConfiguration.scanConfiguration(getClass)
  
  lazy val configuracao : TipoConfiguracao = {
    val comps = configuracao1.values.toSeq.
                  map(c => TipoConfiguracaoComponente(c.groupId + "/" + c.artifactId, c.build.toString))          
    TipoConfiguracao(comps : _*)
  }
}
