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

  /**
    * Given key/value, returns a map of (string, byte array)
    *
    * ==== Why return a map? Not a single byte array? ====
    *  When folded, the given key/value should be represented into multiple key, value pairs.
    *
    *  For string example:
    *  {{{
    *  "x" -> "5Hello" with Q = 2 (folding by two bytes)
    *
    *  =>
    *
    *  "x" -> "5H"
    *  "x1" -> "el"
    *  "x2" -> "lo"
    *  }}}
    *
    *  For float example:
    *  {{{
    *  "z" -> ABCD (ABCD represents bytes for encoded value)
    *
    *  =>
    *
    *  "z" -> AB
    *  "z1" -> CD
    *  }}}
    *
    *  For decoder, we know x is a string, and by retrieving the first element (5H) we know that we need
    *  retrieve two more elements.
    *  We also know z is a float, so we retrieve one more element to make a floating point number.
    *
    * @param key
    * @param value
    * @param Q
    * @return
    */
  def adjustByteArray(key:String, value:Array[Byte], Q:Int) : Map[String, Array[Byte]] = {
    val result = MMap[String, Array[Byte]]()
    val size = value.size
    if (size == Q) result += (key -> value)
    else if (size < Q) { // patching
      result += (key -> util.conversion.ByteArrayTool.adjust(value = value, goalSize = Q))
    }
    else { // folding
      val (count, remainder) = (size/Q, size%Q)

      for (index <- 0 until count) {
        val keyName = if (index == 0) key else s"${key}${index}"
        result += (keyName -> value.slice(Q * index, Q * (index + 1)))
      }
      if (remainder != 0) {
        val keyName = s"${key}${count}"
        val finalSlice = value.slice(Q * count, size)
        val finalByteArray = util.conversion.ByteArrayTool.adjust(value = finalSlice, goalSize = Q)
        result += (keyName -> finalByteArray)
      }
    }

    result.toMap
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
