package bloomierfilter.main

import conversion.{Decoder, Encoder}
import scala.collection.mutable.{Map => MMap}

object BloomierFilter {
  val USER_PROVIDED_TYPE_DIRECTORY = ""
  val typeInstances: Map[String, chitchat.types.Base[_]] = util.types.Util.getTypeInstances(USER_PROVIDED_TYPE_DIRECTORY)
  val typeInference = new util.types.TypeInference(typeInstances)
  val encoder = new Encoder(typeInference)
  val decoder = new Decoder(typeInference)

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
        val adjusted = adjustByteArray(key, byteArray, Q)
        result ++= adjusted
      }
    }
    // 2. fold the map
    result.toMap
  }

  def adjustByteArray(key:String, value:Array[Byte], Q:Int) : Map[String, Array[Byte]] = {
    null
  }

  def apply(inputAny:Map[String, Any], initialm:Int = 0, k:Int = 3, q:Int,
            maxTry: Int = 5, initialHashSeed:Int = 0, caseSensitive: Boolean = false) = {
    val bf = new BloomierFilter(inputAny = inputAny, initialm = initialm, k = k, q = q,
            maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
    bf
  }
}

class BloomierFilter(val inputAny:Map[String, Any],
                     override val initialm:Int = 0, override val k:Int = 3, override val q:Int,
                     override val maxTry: Int = 5, override val initialHashSeed:Int = 0, override val caseSensitive: Boolean = false)
  extends ByteArrayBloomierFilter(input = BloomierFilter.mapConversion(inputAny, q), initialm = initialm, k = k, q = q,
                                        maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
{


  //  val decoder = new conversion.Decoder(typeInstances)
//  val encoder = new conversion.Encoder(typeInstances)
//
//  val bytearrayInput = encoder.encode(input)
//  val bytearrayBloomierFilter = new ByteArrayBloomierFilter(bytearrayInput,
//    initialm = initialm, k = k, q = q,
//    maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
//
//  val Q = bytearrayBloomierFilter.Q
//  val m = bytearrayBloomierFilter.m
//  val hashSeed = bytearrayBloomierFilter.hashSeed
//
//  def loadUserTypeInstances(directory:String) = {
//    typeInstances ++= util.types.Util.getUserTypeInstances(directory)
//    encoder.typeInstances = typeInstances
//    decoder.typeInstances = typeInstances
//  }
//
//  def get(key:String) = {
//    val result = bytearrayBloomierFilter.getByteArray(key)
//    if (result.isEmpty) None
//    else {
//      decoder.decode(key = key, value = result.get, size = Q)
//    }
//  }
}
