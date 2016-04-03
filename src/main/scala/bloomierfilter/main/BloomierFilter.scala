package bloomierfilter.main

import conversion.{Decoder, Encoder}
import scala.collection.mutable.{Map => MMap}

object BloomierFilter {
  def apply() = {

  }
}

class BloomierFilter(val input:Map[String, Any],
                     val initialm:Int, val k:Int, val q:Int,
                     val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = true)
{
  val decoder = new conversion.Decoder
  val encoder = new conversion.Encoder


  val bytearrayInput = encoder.encode(input)
  val bytearrayBloomierFilter = new ByteArrayBloomierFilter(bytearrayInput,
    initialm = initialm, k = k, q = q,
    maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
  val Q = bytearrayBloomierFilter.Q
  var typeInstances: Map[String, chitchat.types.Base[_]] = util.types.Util.getSystemTypeInstances

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
