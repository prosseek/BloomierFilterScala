package bloomierfilter.main

import java.lang.{String => JString}

import chitchat.typetool.TypeInference

import scala.{Byte => SByte, Float => SFloat, Int => SInt}
import scala.collection.mutable.{Map => MMap}

/**
  *
  * ==== Why the object has only encoder, not decoder? ====
  *  1. The encoder static method is required to transform Map[String, Val] to Map[String, Array[Byte]]
  *  2. The decoder needs a BF's table, but this is available only when the BF is instantiated.
  *  3. So, the encoder in BF uses the encoder int this object, when decoder is a method in the BF.
  */
object BloomierFilter {

  def apply(inputAny:Map[String, Any],
            typeInference: TypeInference,
            initialm:Int = 0, k:Int = 3, q:Int,
            force_depth_count_1:Boolean = false,
            maxTry: Int = 5, initialHashSeed:Int = 0, caseSensitive: Boolean = false) =
  {
    val bf = new BloomierFilter(inputAny = inputAny, typeInference = typeInference,
      initialm = initialm, k = k, q = q,
      force_depth_count_1 = force_depth_count_1,
      maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
    bf
  }

  /**
    * encodes the Map[String, Any] into Map[String, Array[Byte]] with the
    * given q.
    *
    * ==== Things to consider ====
    *  1. q (Q*8) can be larger or smaller than each value type represents.
    *  2. we need to fold or patch
    *
    * ==== Why not using map for conversion ====
    *  This converter is for FBF, so when the Q is smaller than the encoded output, the output should be folded.
    *  `zeroPatchByteArray` retuns a map, not a single byte array, so we need to patch them all.
    *
    * @return converted map
    */
  def mapConversion(inputAny:Map[JString, Any], q:Int, typeInference: TypeInference) : Map[String, Array[Byte]] = {
    val Q = util.conversion.Util.getBytesForBits(q)

    //    val result = MMap[JString, Array[Byte]]()
    //    // if each element is longer than Q, fold it.
    //    // if shorter than Q, patch it with 0s.
    //    inputAny foreach {
    //      case (label, value) => {
    //        val byteArray = typeInference.encode(label, value)
    //        if (byteArray.isDefined)
    //          result ++= bloomierfilter.conversion.Util.zeroPatchByteArray(label, byteArray.get, Q)
    //        else
    //          throw new RuntimeException(s"Error in map conversion ${label}/${value}")
    //      }
    //    }
    //    result.toMap

    (Map[JString, Array[Byte]]() /: inputAny) { (acc, value) => {
        val label = value._1
        val any = value._2
        val byteArray = typeInference.encode(label, any)
        if (byteArray.isDefined)
          acc ++ bloomierfilter.conversion.Util.zeroPatchByteArray(label, byteArray.get, Q)
        else
          throw new RuntimeException(s"Error in map conversion ${label}/${value}")
      }
    }
  }
}

class BloomierFilter(val inputAny:Map[String, Any],
                     val typeInference: TypeInference,
                     override val initialm:Int = 0, override val k:Int = 3, override val q:Int,
                     override val force_depth_count_1:Boolean = false,
                     override val maxTry: Int = 5, override val initialHashSeed:Int = 0, override val caseSensitive: Boolean = false)
  extends ByteArrayBloomierFilter(input = BloomierFilter.mapConversion(inputAny = inputAny,
                                                                       typeInference = typeInference, q = q),
                                  initialm = initialm, k = k, q = q,
                                  force_depth_count_1 = force_depth_count_1,
                                  maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
{
  /**
    * Retrieve the partial bytearrays (size Q) up to given size bytes
    *
    * ==== Why this function ====
    *  This is for Folded Bloomier Filter.
    *  When we have Q smaller than size, we need to access the table multiple times to get partial byte arrays.
    *  When return, the retrieved byte arrays should be assemembled.
    *
    * ==== Example ====
    * {{{
    *   Q = 2
    *   size = 6
    *   key = "hey"
    *
    *   We should retrieve 3 times to get all the values.
    *   hey -> "ab"
    *   hey1 -> "xy"
    *   hey2 -> "kw"
    *
    *   return => "ab" + "xy" + "kw"
    * }}}
    *
    * @param size
    * @return
    */
  def getFullByteArray(key:JString, size:Int) : Array[SByte] = {
    val (count, remainder) = (size / Q, size % Q)
    val totalCount =
      if (count == 0) 1 // get only one time if Q > size
      else {
        if (remainder == 0) count else count + 1
      }

    // the index is from 0 until totalCount
    // for example, size = 10, Q = 3 => count = 3, remainder = 1, totalCount = 4
    // the index ranges from 0 until 4 (0,1,2,3)
    //
    // another example, size = 10, Q = 5 => count = 2, remainder = 0, totalCount = 2
    // the index range from 0 until 2 (0, 1)

    var byteArray = Array[SByte]()
    for (index <- 0 until totalCount) {
      val numberedKey:JString = if (index == 0) key else s"${key}${index}"
      byteArray ++= getByteArray(numberedKey).get
    }

    byteArray
  }
}
