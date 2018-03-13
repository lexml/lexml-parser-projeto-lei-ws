package br.gov.lexml.parser.pl.ws.actors

import java.io.{File, FileFilter}

import akka.actor.Actor
import br.gov.lexml.parser.pl.ws.ServiceParams
import grizzled.slf4j.Logging
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

trait ResultCleaner {
  self: Logging ⇒
  def delete(incomplete: Boolean, toleranceMinutes: Int): Unit = {
    val dir = ServiceParams.params.parseResultDirectory
    self.logger.info("scanning " + dir.getPath + " for obsolete result dirs. " +
      "Considering incomplete = " + incomplete +
      ", tolerance = " + toleranceMinutes + " minutes")
    import FileFilterUtils._
    val tolerance = toleranceMinutes * 60L * 1000L
    val filter = and(
      directoryFileFilter, ageFileFilter(System.currentTimeMillis() - tolerance, true), if (incomplete) { ResultCleaner.containsWaitFileFilter } else {
        notFileFilter(ResultCleaner.containsWaitFileFilter)
      })
    for { l <- Option(dir.listFiles(filter : FileFilter)) ; f ← l } {
      self.logger.info("cleaning " + f.getCanonicalPath)
      FileUtils.deleteQuietly(f)
    }
  }
}

object ResultCleaner {

  val containsWaitFileFilter: AbstractFileFilter {
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
  lazy val tolerance: Int = ServiceParams.params.incompleteCleaningIntervalMinutes
  override def preStart() {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2.0) + " minutes")
    implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher
    context.system.scheduler.schedule((tolerance * 60) seconds, (tolerance * 30) seconds, self, CleanIt)
  }
  def receive: PartialFunction[Any, Unit] = {
    case CleanIt ⇒
      val incomplete = true
      delete(incomplete, ServiceParams.params.incompleteCleaningIntervalMinutes)
  }
}

class CompleteCleanActor extends Actor with Logging with ResultCleaner {
  lazy val tolerance: Int = ServiceParams.params.completeCleaningIntervalMinutes
  override def preStart() {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2.0) + " minutes")
    implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher
    context.system.scheduler.schedule((tolerance * 60) seconds, (tolerance * 30) seconds, self, CleanIt)
  }
  def receive: PartialFunction[Any, Unit] = {
    case CleanIt ⇒
      val complete = false
      delete(complete, ServiceParams.params.completeCleaningIntervalMinutes)

  }
}