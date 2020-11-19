package br.gov.lexml.parser.pl.ws

import java.util.concurrent.TimeUnit

import com.hazelcast.config.MaxSizePolicy
import grizzled.slf4j.Logging

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

class Cache extends Logging {
  import com.hazelcast.config.Config
  import com.hazelcast.map.IMap
  import com.hazelcast.core.Hazelcast
  import com.hazelcast.core.HazelcastInstance

/*  import com.hazelcast.core.EntryEvent
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
  } */

  val hazelcastCfg = new Config()
  val clusterName = LexmlWsConfig.config.getString("hazelcast.cluster-name")
  logger.info(s"Creating Hazelcast cluster name='$clusterName'")
  hazelcastCfg.setClusterName(clusterName) //"lexml-parser"
  hazelcastCfg.setProperty("hazelcast.logging.type", "slf4j")
  private val mCfg = hazelcastCfg.getMapConfig("results")
  val ttl = LexmlWsConfig.config.getDuration("hazelcast.time-to-live",TimeUnit.SECONDS).toInt
  logger.info(s"Setting time-to-live to $ttl")
  mCfg.setTimeToLiveSeconds(ttl) // 300 seconds
  val maxIdle = LexmlWsConfig.config.getDuration("hazelcast.max-idle-time",TimeUnit.SECONDS).toInt
  logger.info(s"Setting max idle time to $maxIdle")
  mCfg.setMaxIdleSeconds(maxIdle)
  private val evCfg = mCfg.getEvictionConfig
  evCfg.setMaxSizePolicy(MaxSizePolicy.PER_NODE)
  val maxSize = LexmlWsConfig.config.getInt("hazelcast.max-size")
  logger.info(s"Setting max size to $maxSize")
  evCfg.setSize(maxSize)
  val hazelcastInstance: HazelcastInstance = Hazelcast.newHazelcastInstance(hazelcastCfg)
  logger.info("Hazelcast instance created")
  val resultMap : IMap[CacheKey,CacheElem] = hazelcastInstance.getMap("results").asInstanceOf[IMap[CacheKey,CacheElem]]
  logger.info("result map created")
//  resultMap.addEntryListener(new MyEntryListener,true)


}

object Cache {
  private var _cache : Option[Cache] = None
  def init(): Unit = {
    _cache = Some(new Cache())
  }
  def apply() : Cache = _cache.getOrElse(throw new RuntimeException("Cache accessed before initialization!"))
}


