package bloomierfilter.main

import bloomierfilter.conversion.{Decoder, Encoder}
import scala.collection.mutable.{Map => MMap}

/**
  *
  * ==== Why the object has only encoder, not decoder? ====
  *  1. The encoder static method is required to transform Map[String, Val] to Map[String, Array[Byte]]
  *  2. The decoder needs a BF's table, but this is available only when the BF is instantiated.
  *  3. So, the encoder in BF uses the encoder int this object, when decoder is a method in the BF.
  */
object BloomierFilter {
  val USER_PROVIDED_TYPE_DIRECTORY = ""
  val typeInstances: Map[String, chitchat.types.Base[_]] = util.types.Util.getTypeInstances(USER_PROVIDED_TYPE_DIRECTORY)
  val typeInference = new util.types.TypeInference(typeInstances)
  val encoder = new Encoder(typeInference)

  /**
    * encodes the Map[String, Any] into Map[String, Array[Byte]] with the
    * given q.
    *
    * ==== Things to consider ====
    *  1. q (Q*8) can be larger or smaller than each value type represents.
    *  2. we need to fold or patch
    *
    * @param input
    * @return converted map
    */
  def mapConversion(input:Map[String, Any], q:Int) : Map[String, Array[Byte]] = {
    val encoder = new Encoder(typeInference)
    val Q = util.conversion.Util.getBytesForBits(q)

    // 1. get the width from Q for folding
    val encodedByteArray = encoder.encode(input)

    val result = MMap[String, Array[Byte]]()
    // if each element is longer than Q, fold it.
    // if shorter than Q, patch it with 0s.
    encodedByteArray foreach {
      case (key, byteArray) => {
        val adjusted = bloomierfilter.conversion.Util.zeroPatchByteArray(key, byteArray, Q)
        result ++= adjusted
      }
    }
    // 2. fold the map
    result.toMap
  }
}

class BloomierFilter(val inputAny:Map[String, Any],
                     override val initialm:Int = 0, override val k:Int = 3, override val q:Int,
                     override val maxTry: Int = 5, override val initialHashSeed:Int = 0, override val caseSensitive: Boolean = false)
  extends ByteArrayBloomierFilter(input = BloomierFilter.mapConversion(inputAny, q),
                                  initialm = initialm, k = k, q = q,
                                  maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
{
  val encoder = BloomierFilter.encoder
  val decoder = new Decoder(BloomierFilter.typeInference, this)
}
