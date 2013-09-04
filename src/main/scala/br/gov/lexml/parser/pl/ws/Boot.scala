package br.gov.lexml.parser.pl.ws

import akka.actor.Actor
import akka.actor.Actor._
import scala.xml.NodeSeq
import java.lang.Integer
import java.nio.ByteBuffer
import javax.ws.rs.core.MultivaluedMap
import javax.ws.rs.{GET, POST, Path, Produces, WebApplicationException, Consumes,PathParam}
import br.gov.lexml.parser.pl.ws.resources.ParserServiceActor
import br.gov.lexml.parser.pl.ws.actors.IncompleteCleanActor
import br.gov.lexml.parser.pl.ws.actors.CompleteCleanActor
import akka.actor.ActorRef
import java.util.concurrent.TimeUnit.SECONDS
import grizzled.slf4j.Logging
import Actor._
import akka.routing._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.OneForOneStrategy

class Boot {

  val system = ActorSystem("lexml-parser-system")
  
  
  import akka.actor.SupervisorStrategy._
  
  val parserServiceSupervisionStrategy = OneForOneStrategy() {
    case ex : Exception => Restart
  }
  
  val parserServiceRouter = system.actorOf(Props[ParserServiceActor].
      withRouter(SmallestMailboxRouter(
          nrOfInstances=8, supervisorStrategy = parserServiceSupervisionStrategy)))
  
  import akka.actor.ActorDSL._          
          
  val incompleteCleanActor = system.actorOf(Props[IncompleteCleanActor])
  val completeCleanActor = system.actorOf(Props[CompleteCleanActor])
    
  Mime.init()
}

