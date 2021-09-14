package br.gov.lexml.parser.pl.ws

import java.util.concurrent.TimeUnit

import be.geertvanheusden.hazelcast.discovery.RancherDiscoveryStrategyFactory
import com.hazelcast.config.MaxSizePolicy
import com.hazelcast.config.DiscoveryStrategyConfig
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

  val config = new Config()
  val clusterName = LexmlWsConfig.config.getString("hazelcast.cluster-name")
  info(s"Creating Hazelcast cluster name='$clusterName'")
  config.setClusterName(clusterName) //"lexml-parser"
  config.setProperty("hazelcast.logging.type", "slf4j")

  if(LexmlWsConfig.config.getString("hazelcast.discovery") == "rancher") {
    info("Hazelcast discovery using Rancher metadata api")

    //config.setProperty("hazelcast.discovery.enabled", "true")

    val joinConfig = config.getNetworkConfig.getJoin
    joinConfig.getMulticastConfig.setEnabled(false)
    val tcpIpConfig = joinConfig.getTcpIpConfig()
    val cluster_ips_env = Option(System.getenv("cluster_ips"))
    val cluster_ips_prop = Option(System.getProperty("cluster_ips",null))
    val cluster_ips_opt = cluster_ips_prop.orElse(cluster_ips_env)
    info(s"cluster_ips_env=$cluster_ips_env")
    info(s"cluster_ips_prop=$cluster_ips_prop")
    info(s"cluster_ips_opt=$cluster_ips_opt")
    for {
      cluster_ips <- cluster_ips_opt
      ip <- cluster_ips.split(" +")
    } {
      info(s"Adding hazelcast member: $ip")
      tcpIpConfig.addMember(ip.trim)
    }

    tcpIpConfig.setConnectionTimeoutSeconds(60)
    tcpIpConfig.setEnabled(true)

   /* val discoveryConfig = joinConfig.getDiscoveryConfig

    val factory = new RancherDiscoveryStrategyFactory()
    val strategyConfig = new DiscoveryStrategyConfig(factory)

    discoveryConfig.addDiscoveryStrategyConfig(strategyConfig) */
  } else {
    info("Hazelcast discovery using default strategy")
  }

  private val mCfg = config.getMapConfig("results")
  val ttl = LexmlWsConfig.config.getDuration("hazelcast.time-to-live",TimeUnit.SECONDS).toInt
  info(s"Setting time-to-live to $ttl")
  mCfg.setTimeToLiveSeconds(ttl) // 300 seconds
  val maxIdle = LexmlWsConfig.config.getDuration("hazelcast.max-idle-time",TimeUnit.SECONDS).toInt
  info(s"Setting max idle time to $maxIdle")
  mCfg.setMaxIdleSeconds(maxIdle)
  private val evCfg = mCfg.getEvictionConfig
  evCfg.setMaxSizePolicy(MaxSizePolicy.PER_NODE)
  val maxSize = LexmlWsConfig.config.getInt("hazelcast.max-size")
  info(s"Setting max size to $maxSize")
  evCfg.setSize(maxSize)
  val hazelcastInstance: HazelcastInstance = Hazelcast.newHazelcastInstance(config)
  info("Hazelcast instance created")
  val resultMap : IMap[CacheKey,CacheElem] = hazelcastInstance.getMap("results").asInstanceOf[IMap[CacheKey,CacheElem]]
  info("result map created")
//  resultMap.addEntryListener(new MyEntryListener,true)


}

object Cache {
  private var _cache : Option[Cache] = None
  def init(): Unit = {
    _cache = Some(new Cache())
  }
  def apply() : Cache = _cache.getOrElse(throw new RuntimeException("Cache accessed before initialization!"))
}


