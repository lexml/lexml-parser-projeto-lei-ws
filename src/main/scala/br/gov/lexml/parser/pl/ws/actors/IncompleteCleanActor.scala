package br.gov.lexml.parser.pl.ws.actors

import akka.actor.Actor

import scala.language.postfixOps
import akka.actor.Actor._
import grizzled.slf4j.Logging
import br.gov.lexml.parser.pl.ws.ServiceParams
import org.apache.commons.io.filefilter._
import org.apache.commons.io.FileUtils
import java.io.File
import java.io.FileFilter
import akka.actor.Scheduler
import java.util.concurrent.TimeUnit.SECONDS
import akka.actor.Scheduler
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

trait ResultCleaner {
  self: Logging ⇒
  def delete(incomplete: Boolean, toleranceMinutes: Int) = {
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
      self.logger.info("cleaning " + f.getCanonicalPath())
      FileUtils.deleteQuietly(f)
    }
  }
}

object ResultCleaner {

  val containsWaitFileFilter = new AbstractFileFilter {
    override def accept(f: File) = {
      val g = new File(f, "wait")
      g.exists()
    }
  }

}

case object CleanIt

class IncompleteCleanActor extends Actor with Logging with ResultCleaner {
  lazy val tolerance = ServiceParams.params.incompleteCleaningIntervalMinutes
  override def preStart() {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2.0) + " minutes")
    implicit val dispatcher = context.system.dispatcher
    context.system.scheduler.schedule((tolerance * 60) seconds, (tolerance * 30) seconds, self, CleanIt)
  }
  def receive = {
    case CleanIt ⇒ {
      delete(true, ServiceParams.params.incompleteCleaningIntervalMinutes)
    }
  }
}

class CompleteCleanActor extends Actor with Logging with ResultCleaner {
  lazy val tolerance = ServiceParams.params.completeCleaningIntervalMinutes
  override def preStart() {
    logger.info("scheduling " + self.path + " to start in " + tolerance +
      " minutes, firing up again every " + (tolerance / 2.0) + " minutes")
    implicit val dispatcher = context.system.dispatcher
    context.system.scheduler.schedule((tolerance * 60) seconds, (tolerance * 30) seconds, self, CleanIt)
  }
  def receive = {
    case CleanIt ⇒ {
      delete(false, ServiceParams.params.completeCleaningIntervalMinutes)
    }
  }
}