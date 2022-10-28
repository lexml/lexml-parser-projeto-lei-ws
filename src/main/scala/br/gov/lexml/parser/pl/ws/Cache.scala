package br.gov.lexml.parser.pl.ws

import java.util.concurrent.TimeUnit

import grizzled.slf4j.Logging

final case class CacheElem(mimeType : String, fileName : String, contents : Array[Byte]) extends Serializable {
  override def toString() : String = s"CacheElem(mimeType = '$mimeType', fileName = '$fileName'', contents.length = ${contents.length})"
}
final case class CacheKey(comps : Vector[String]) extends Serializable {
  def +(k : String) : CacheKey = CacheKey(comps :+ k)
  def ++(ks : Iterable[String]) : CacheKey = CacheKey(comps ++ ks)
  override def toString() : String = comps.mkString("/")
}
object CacheKey {
  def apply(k : String) : CacheKey = CacheKey(Vector(k))
}

class DataCache extends Logging {
  import com.github.benmanes.caffeine.cache._

  private val cache : Cache[CacheKey,CacheElem] =
    Caffeine.newBuilder()
      .expireAfterWrite(5,TimeUnit.MINUTES)
      .build[CacheKey,CacheElem]()

  private val cleanupThread = new Thread(() =>
    while(true) {
      Thread.sleep(TimeUnit.MINUTES.toMillis(5))
      logger.info(s"Cleaning up cache: size = ${cache.estimatedSize()}")
      cache.cleanUp()
      logger.info(s"After clean up, cache: size = ${cache.estimatedSize()}")
    })
  cleanupThread.setDaemon(true)
  cleanupThread.start()
  def put(key : CacheKey, elem : CacheElem) : Unit = {
    cache.put(key,elem)
    logger.info(s"Adding element to cache, key=${key}, size now is ${cache.estimatedSize()}")
  }

  def get(key : CacheKey) : Option[CacheElem] = {
    val x = Option(cache.getIfPresent(key))
    if(logger.isInfoEnabled) {
      x match {
        case None =>
          logger.info(s"Cache miss, key=${key}")
          logger.info(s"Cache = ${cache.asMap()}")
        case Some(y) =>
          logger.info(s"Cache hit, key=${key}, fileName = ${y.fileName}, mimeType = ${y.mimeType}, fileSize = ${y.contents.length}")
      }
    }
    x
  }

  def has(key : CacheKey) : Boolean = {
    val res = get(key).isDefined
    logger.info(s"checking cache for key=${key}: ${res}")
    res
  }

  def delete(key : CacheKey) = {
    logger.info(s"removing key=${key} from cache")
    cache.invalidate(key)
  }
}

object DataCache {
  private var _cache : Option[DataCache] = None
  def init(): Unit = {
    _cache = Some(new DataCache())
  }
  def apply() : DataCache =
    _cache.getOrElse(throw new RuntimeException("Cache accessed before initialization!"))
}


