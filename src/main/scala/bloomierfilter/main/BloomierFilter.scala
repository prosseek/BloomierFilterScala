package bloomierfilter.main

import conversion.{Decoder, Encoder}
import scala.collection.mutable.{Map => MMap}

object BloomierFilter {
  def apply() = {

  }
}

class BloomierFilter(val input:Map[String, Any],
                     val initialm:Int = 0, val k:Int = 3, val q:Int,
                     val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = false)
{
  var typeInstances: Map[String, chitchat.types.Base[_]] = util.types.Util.getSystemTypeInstances
  val decoder = new conversion.Decoder(typeInstances)
  val encoder = new conversion.Encoder(typeInstances)

  val bytearrayInput = encoder.encode(input)
  val bytearrayBloomierFilter = new ByteArrayBloomierFilter(bytearrayInput,
    initialm = initialm, k = k, q = q,
    maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)

  val Q = bytearrayBloomierFilter.Q
  val m = bytearrayBloomierFilter.m
  val hashSeed = bytearrayBloomierFilter.hashSeed

  def loadUserTypeInstances(directory:String) = {
    typeInstances ++= util.types.Util.getUserTypeInstances(directory)
    encoder.typeInstances = typeInstances
    decoder.typeInstances = typeInstances
  }

  def get(key:String) = {
    val result = bytearrayBloomierFilter.getByteArray(key)
    if (result.isEmpty) None
    else {
      decoder.decode(key = key, value = result.get, size = Q)
    }
  }
}
