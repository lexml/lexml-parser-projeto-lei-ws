package br.gov.lexml.parser.pl.ws

import java.util.concurrent.TimeUnit

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

class DataCache extends Logging {
  import com.github.benmanes.caffeine.cache._

  private val cache : Cache[CacheKey,CacheElem] =
    Caffeine.newBuilder()
      .expireAfterWrite(5,TimeUnit.MINUTES)
      .build[CacheKey,CacheElem]()

  private val cleanupThread = new Thread(() =>
    while(true) {
      Thread.sleep(TimeUnit.MINUTES.toMillis(5));
      cache.cleanUp()
    })
  cleanupThread.setDaemon(true)
  cleanupThread.start()
  def put(key : CacheKey, elem : CacheElem) : Unit = {
    cache.put(key,elem)
  }

  def get(key : CacheKey) : Option[CacheElem] = {
    Option(cache.getIfPresent(key))
  }

  def has(key : CacheKey) : Boolean = get(key).isDefined

  def delete(key : CacheKey) =
    cache.invalidate(key)
}

object DataCache {
  private var _cache : Option[DataCache] = None
  def init(): Unit = {
    _cache = Some(new DataCache())
  }
  def apply() : DataCache = _cache.getOrElse(throw new RuntimeException("Cache accessed before initialization!"))
}


