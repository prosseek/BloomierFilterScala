package bloomierfilter.main

import conversion.{Decoder, Encoder}
import scala.collection.mutable.{Map => MMap}

object BloomierFilter {
  val USER_PROVIDED_TYPE_DIRECTORY = ""
  val typeInstances: Map[String, chitchat.types.Base[_]] = util.types.Util.getTypeInstances(USER_PROVIDED_TYPE_DIRECTORY)
  val typeInference = new util.types.TypeInference()
  val encoder = new Encoder(typeInstances)

  def mapConversion(input:Map[String, Any]) : Map[String, Array[Byte]] = {
    val res = MMap[String, Array[Byte]]()
    val encoder = new Encoder(null)
    input foreach {
      case (key, value) => res(key) = encoder.encode(key, value)
    }

    res.toMap
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
  extends FoldedByteArrayBloomierFilter(input = BloomierFilter.mapConversion(inputAny), initialm = initialm, k = k, q = q,
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
