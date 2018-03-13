package br.gov.lexml.parser.pl.ws

import akka.actor.{ActorSystem, OneForOneStrategy, Props}
import akka.routing._
import br.gov.lexml.parser.pl.ws.actors.{CompleteCleanActor, IncompleteCleanActor}
import br.gov.lexml.parser.pl.ws.resources.ParserServiceActor

class Boot {

  val system = ActorSystem("lexml-parser-system")
  
  
  import akka.actor.SupervisorStrategy._
  
  val parserServiceSupervisionStrategy = OneForOneStrategy() {
    case ex : Exception => Restart
  }
  
  val parserServiceRouter = system.actorOf(Props[ParserServiceActor].
      withRouter(SmallestMailboxRouter(
          nrOfInstances=8, supervisorStrategy = parserServiceSupervisionStrategy)))
          
  val incompleteCleanActor = system.actorOf(Props[IncompleteCleanActor])
  val completeCleanActor = system.actorOf(Props[CompleteCleanActor])
    
  MimeExtensionRegistry.init()
}

