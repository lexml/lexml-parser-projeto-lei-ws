package br.gov.lexml.parser.pl.ws

import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.routing._
import br.gov.lexml.parser.pl.ws.resources.ParserServiceActor
import com.hazelcast.config.MaxSizePolicy
import com.hazelcast.map.listener.EntryExpiredListener
import io.prometheus.client.hotspot.DefaultExports

final case class CacheElem(mimeType : String, fileName : String, contents : Array[Byte]) extends Serializable {
  override def toString() : String = s"CacheElem(mimeType = '$mimeType', fileName = '$fileName'', contents.length = ${contents.length})"
}
final case class CacheKey(comps : Array[String]) extends Serializable {
  def +(k : String) : CacheKey = CacheKey(comps :+ k)
  def ++(ks : Iterable[String]) : CacheKey = CacheKey(comps ++ ks)
  override def toString() : String = comps.mkString("/")
}
object CacheKey {
  def apply(k : String) : CacheKey = CacheKey(Array(k))
}

class Cache {
  import com.hazelcast.config.Config
  import com.hazelcast.map.IMap
  import com.hazelcast.core.Hazelcast
  import com.hazelcast.core.HazelcastInstance

  import com.hazelcast.core.EntryEvent
  import com.hazelcast.map.MapEvent
  import com.hazelcast.map.listener.EntryAddedListener
  import com.hazelcast.map.listener.EntryEvictedListener
  import com.hazelcast.map.listener.EntryLoadedListener
  import com.hazelcast.map.listener.EntryRemovedListener
  import com.hazelcast.map.listener.EntryUpdatedListener
  import com.hazelcast.map.listener.MapClearedListener
  import com.hazelcast.map.listener.MapEvictedListener

  class MyEntryListener extends EntryAddedListener[CacheKey, CacheElem]
    with EntryRemovedListener[CacheKey, CacheElem]
    with EntryUpdatedListener[CacheKey, CacheElem]
    with EntryEvictedListener[CacheKey, CacheElem]
    with EntryLoadedListener[CacheKey, CacheElem]
    with MapEvictedListener
    with MapClearedListener
    with EntryExpiredListener[CacheKey, CacheElem] {
    override def entryAdded(event: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.out.println("Entry Added:" + event)
    }

    override def entryRemoved(event: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.out.println("Entry Removed:" + event)
    }

    override def entryUpdated(event: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.out.println("Entry Updated:" + event)
    }

    override def entryEvicted(event: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.out.println("Entry Evicted:" + event)
    }

    override def entryLoaded(event: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.out.println("Entry Loaded:" + event)
    }

    override def mapEvicted(event: MapEvent): Unit = {
      System.out.println("Map Evicted:" + event)
    }

    override def mapCleared(event: MapEvent): Unit = {
      System.out.println("Map Cleared:" + event)
    }

    override def entryExpired(entryEvent: EntryEvent[CacheKey, CacheElem]): Unit = {
      System.err.println("Entry expired:" + entryEvent)
    }
  }

  val hazelcastCfg = new Config()
  hazelcastCfg.setClusterName("lexml-parser")
  private val mCfg = hazelcastCfg.getMapConfig("results")
  mCfg.setTimeToLiveSeconds(300)
  mCfg.setMaxIdleSeconds(120)
  private val evCfg = mCfg.getEvictionConfig
  evCfg.setMaxSizePolicy(MaxSizePolicy.PER_NODE)
  evCfg.setSize(100)
  val hazelcastInstance: HazelcastInstance = Hazelcast.newHazelcastInstance(hazelcastCfg)
  val resultMap : IMap[CacheKey,CacheElem] = hazelcastInstance.getMap("results").asInstanceOf[IMap[CacheKey,CacheElem]]
  resultMap.addEntryListener(new MyEntryListener,true)


}

class Boot {

  val cache = new Cache()

  val system: ActorSystem = ActorSystem("lexml-parser-system")
  
  
  import akka.actor.SupervisorStrategy._
  
  val parserServiceSupervisionStrategy: OneForOneStrategy = OneForOneStrategy() {
    case _ : Exception => Restart
  }
  
  val parserServiceRouter: ActorRef =
    system.actorOf(Props[ParserServiceActor].withRouter(SmallestMailboxPool(8,
    supervisorStrategy = parserServiceSupervisionStrategy)))


  MimeExtensionRegistry.init()
  
  DefaultExports.initialize()
}

