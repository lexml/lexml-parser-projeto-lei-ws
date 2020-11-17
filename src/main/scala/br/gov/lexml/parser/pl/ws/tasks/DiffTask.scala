package br.gov.lexml.parser.pl.ws.tasks

import com.typesafe.config.ConfigFactory
import java.lang.reflect.Method

import grizzled.slf4j.{Logger, Logging}

trait DiffTask {
  def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String) :
	  Option[(Array[Byte],Option[Int])]
}

object DiffTask extends Logging {   
  lazy val diffTask: DiffTask = {
    DelegatedDiffTask() match {
      case Some(dt) =>
        logger.info("DelegatedDiffTask created")
        dt
      case None =>
        logger.info("DelegatedDiffTask not created")
        NullDiffTask
    }       
  }
}

object NullDiffTask extends DiffTask {
  override def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String): Option[(Array[Byte], Option[Int])] = None
}

class DelegatedDiffTask(clazz : Class[_], m : Method) extends DiffTask {  
  import DelegatedDiffTask.logger
  
  logger.debug("instance: creating")  
  val instance: Any = try {
    clazz.getDeclaredConstructor().newInstance()
  } catch {
    case ex: Exception =>
      logger.error("Error creating instance",ex)
      throw ex
  }
  logger.debug(s"instance: created: $instance")
    
  override def buildDiff(from : Array[Byte], fromMime : String, to : Array[Byte], toMime : String): Option[(Array[Byte], Option[Int])] = {
    try {
      logger.debug(s"buildDiff: from.length=${from.length}, to.length=${to.length}")          
      val res = try {
        m.invoke(instance, from, fromMime, to, toMime).asInstanceOf[Option[(Array[Byte],Option[Int])]]
      } catch {
        case ex: Exception =>
          logger.error("buildDiff: error",ex)
          throw ex
      }
      logger.debug(s"buildDiff: res=$res")
      res
    } catch {
      case ex : Exception =>
        logger.error(s"Error calling buildDiff method, method=$m, class=${clazz.getName}")
        None
    }
  }
}
  
object DelegatedDiffTask extends Logging {
  override val logger: Logger = super.logger
  def apply() : Option[DiffTask] = {    
    logger.debug("initializing DelegatedDiffTask object")
    lazy val config = ConfigFactory.load().getConfig("lexml.parser.ws.tasks.diff")
    logger.debug(s"config=$config")
    lazy val className = try {
      val cn = Option(config.getString("diff-task-impl-class")).filterNot(_.isEmpty())
      logger.debug(s"cn=$cn")
      if(cn.isEmpty) {
        logger.info("Diff className was not provided. Diff task will be skipped.")
      }
      cn
    } catch { case _ : Exception => None }
    logger.debug(s"className=$className")
    lazy val buildDiffMethod = classOf[DiffTask].getMethods.filter(_.getName == "buildDiff").head
    logger.debug(s"buildDiffMethod=$buildDiffMethod")
    lazy val buildDiffMethodParams = buildDiffMethod.getParameterTypes
    logger.debug(s"buildDiffMethodParams=${buildDiffMethodParams.to(Seq)}")
    for {
      clsName <- className
      _ = logger.debug(s"clsName=$clsName")
      clz <- 
        try {
          Option(
            Thread.currentThread()
              .getContextClassLoader
              .loadClass(clsName) ) 
        } catch { 
          case _ : ClassNotFoundException =>
            logger.info(s"Diff class not found: $clsName")
            None 
        }
      _ = logger.debug(s"clz=$clz")
      canBeInstantiated = try {
          clz.getDeclaredConstructor().newInstance()
          true
        } catch {
          case _ : Exception =>
            logger.error(s"Diff class cannot be instantiated: ${clz.getName}")
            false
        } 
      _ = logger.debug(s"canBeInstantiated=$canBeInstantiated")
      if canBeInstantiated
      m <- 
        try {
          Option(clz.getMethod(buildDiffMethod.getName,buildDiffMethodParams : _*))
        } catch {
          case _ : Exception =>
            logger.error(s"buildDiff method not found in Diff class: ${clz.getName}")
            None  
        }
      _ = logger.debug(s"m=$m")
    } yield {
      new DelegatedDiffTask(clz,m)         
    }
  }     
}

