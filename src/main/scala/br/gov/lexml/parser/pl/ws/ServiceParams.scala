package br.gov.lexml.parser.pl.ws

import java.io.File
import javax.servlet.ServletContext

import br.gov.lexml.parser.pl.ws.data.Componente
import grizzled.slf4j.Logging


class ServiceParams(octx: Option[ServletContext] = None) extends Logging {
  def option[T](name: String, default: => T)(implicit canBuildFrom: String => Option[T]): T = {
    octx match {
      case None =>
        val v: T = default
        logger.info("no context: option '" + name + "' set to default = " + v)
        v
      case Some(ctx) =>
        Option(ctx.getInitParameter(name)) match {
          case None =>
            val v: T = default
            logger.info("context exists: no parameter for option '" + name + "'. setting to default = " + v)
            v

          case Some(str) => canBuildFrom(str) match {
            case None =>
              val v: T = default
              logger.info("context exists, option '" + name + "' is set to '" + str + "'. can't build value. setting to default = " + v)
              v
            case Some(v) =>
              logger.info("context exists, option '" + name + "' is set to '" + str + "'. setting to = " + v)
              v
          }
        }
    }
    //octx.flatMap(ctx => Option(ctx.getInitParameter(name)).flatMap(canBuildFrom)).getOrElse(default)
  }

  import scala.language.implicitConversions

  implicit def canBuildFile(path: String): Option[File] = {
    val f = new File(path).getCanonicalFile
    logger.debug("canBuildFile: path = " + path + ", f = " + f + ", exists = " + f.exists)
    if (f.exists()) {
      Some(f)
    } else {
      None
    }
  }

  implicit def canBuildInt(num: String): Option[Int] =
    try {
      Some(num.toInt)
    } catch {
      case _: Exception => None
    }

  /*val defaultStaticDir: File = try {
    new File("src/main/resources").getCanonicalFile
  } catch {
    case ex: Exception =>
      logger.warn("default static directory not present", ex)
      new File("src/main/resources")
  }*/
//  var staticOverrideDirectory: File = option("staticOverrideDirectory", defaultStaticDir)
}

object ServiceParams {

  var params: ServiceParams = new ServiceParams()

  lazy val configuracao: Vector[Componente] = Vector(
    Componente(nome="parser", versao="1.0")
  )
}
