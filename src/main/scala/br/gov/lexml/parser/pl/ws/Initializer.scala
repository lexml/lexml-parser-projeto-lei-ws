package br.gov.lexml.parser.pl.ws

import javax.servlet.{ServletContext, ServletContextEvent, ServletContextListener}

 /**
  * This class can be added to web.xml mappings as a listener to start and postStop Akka.
  *<web-app>
  * ...
  *  <listener>
  *    <listener-class>br.gov.lexml.parser.pl.ws.Initializer</listener-class>
  *  </listener>
  * ...
  *</web-app>
  */
class Initializer extends ServletContextListener {
   
   def contextDestroyed(e: ServletContextEvent): Unit = Initializer.stop()
   def contextInitialized(e: ServletContextEvent): Unit = Initializer.start(Some(e.getServletContext))    
 }

object Initializer {
  
  var boot : Option[Boot] = None
  
  def start(oe : Option[ServletContext] = None) {
    ServiceParams.params = new ServiceParams(oe)
    
    boot = Some(new Boot())
    
    
  }
  def stop() {
    boot.foreach(_.system.shutdown)
    boot = None
  }
}

