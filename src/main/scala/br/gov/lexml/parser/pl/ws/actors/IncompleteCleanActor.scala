package br.gov.lexml.parser.pl.ws.actors

import java.io.{File, FileFilter}
import akka.actor.Actor
import br.gov.lexml.parser.pl.ws.LexmlWsConfig
import grizzled.slf4j.Logging
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps

trait ResultCleaner {
  self: Logging =>
  def delete(incomplete: Boolean, tolerance : FiniteDuration): Unit = {
    val dir = new File(LexmlWsConfig.appConfig.getString("cache.directory"))
    self.logger.info("scanning " + dir.getPath + " for obsolete result dirs. " +
      "Considering incomplete = " + incomplete +
      ", tolerance = " + tolerance + " minutes")
    import FileFilterUtils._
    val filter = and(
      directoryFileFilter, ageFileFilter(System.currentTimeMillis() - tolerance.toMillis, true), if (incomplete) { ResultCleaner.containsWaitFileFilter } else {
        notFileFilter(ResultCleaner.containsWaitFileFilter)
      })
    for { l <- Option(dir.listFiles(filter : FileFilter)) ; f <- l } {
      self.logger.info("cleaning " + f.getCanonicalPath)
      FileUtils.deleteQuietly(f)
    }
  }
}

object ResultCleaner {

  private val containsWaitFileFilter: AbstractFileFilter {
    def accept(f: File): Boolean
  } = new AbstractFileFilter {
    override def accept(f: File): Boolean = {
      val g = new File(f, "wait")
      g.exists()
    }
  }

}

case object CleanIt

class IncompleteCleanActor extends Actor with Logging with ResultCleaner {
  import scala.jdk.DurationConverters._

  private lazy val tolerance: FiniteDuration = LexmlWsConfig.appConfig.getDuration("cache.incomplete-cleaning-interval").toScala

  override def preStart() : Unit = {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2))
    implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher
    context.system.scheduler.schedule(tolerance, tolerance / 2, self, CleanIt)
  }
  def receive: PartialFunction[Any, Unit] = {
    case CleanIt =>
      val incomplete = true
      delete(incomplete, tolerance)
  }
}

class CompleteCleanActor extends Actor with Logging with ResultCleaner {

  import scala.jdk.DurationConverters._

  private lazy val tolerance: FiniteDuration = LexmlWsConfig.appConfig.getDuration("cache.complete-cleaning-interval").toScala

  override def preStart() : Unit = {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2.0) + " minutes")
    implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher
    context.system.scheduler.schedule(tolerance, tolerance / 2, self, CleanIt)
  }
  def receive: PartialFunction[Any, Unit] = {
    case CleanIt =>
      val complete = false
      delete(complete, tolerance)

  }
}