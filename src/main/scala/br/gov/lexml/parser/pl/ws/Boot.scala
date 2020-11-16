package br.gov.lexml.parser.pl.ws

import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.routing._
import br.gov.lexml.parser.pl.ws.actors.{CompleteCleanActor, IncompleteCleanActor}
import br.gov.lexml.parser.pl.ws.resources.ParserServiceActor
import io.prometheus.client.hotspot.DefaultExports

class Boot {

  val system: ActorSystem = ActorSystem("lexml-parser-system")
  
  
  import akka.actor.SupervisorStrategy._
  
  val parserServiceSupervisionStrategy: OneForOneStrategy = OneForOneStrategy() {
    case _ : Exception => Restart
  }
  
  val parserServiceRouter: ActorRef =
    system.actorOf(Props[ParserServiceActor].withRouter(SmallestMailboxPool(8,
    supervisorStrategy = parserServiceSupervisionStrategy)))


  val incompleteCleanActor: ActorRef = system.actorOf(Props[IncompleteCleanActor])
  val completeCleanActor: ActorRef = system.actorOf(Props[CompleteCleanActor])
    
  MimeExtensionRegistry.init()
  
  DefaultExports.initialize()
}

