package bloomierfilter.main

import java.io.File
import java.lang.{String => JString}

import bloomierfilter.core.{BloomierHasher, Header, Table}
import chitchat.typetool.TypeInference
import chitchat.types.{Encoding, Float, Range, String}

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

  def apply(typeInference: TypeInference, byteArray:Array[SByte]) = {
    val babf = ByteArrayBloomierFilter.apply(byteArray)
    val bf = new BloomierFilter(typeInference = typeInference)
    bf.byteArrayBloomierFilter = babf
    copyParameters(from = babf, to = bf)
  }

  def apply(typeInference: TypeInference, filePath:JString) = {
    if (new File(filePath).exists()) {
      val bf = new BloomierFilter(typeInference = typeInference)
      bf.load(filePath)
      bf
    } else {
      throw new RuntimeException(s"No file ${filePath} exists.")
    }
  }

  def apply(inputAny:Map[JString, Any],
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
  def mapConversion(inputAny:Map[JString, Any], q:Int, typeInference: TypeInference) : Map[JString, Array[SByte]] = {
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

    (Map[JString, Array[SByte]]() /: inputAny) { (acc, value) => {
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
  def copyParameters(from: ByteArrayBloomierFilter, to:BloomierFilter) = {
    to.Q = from.Q
    to.hashSeed = from.hashSeed
    to.n = from.n
    to.non_zero_n = from.non_zero_n
    to.table = from.table
    to.header = from.header
    to.hasher = from.hasher
  }
}

class BloomierFilter(var inputAny:Map[JString, Any] = null,
                     var typeInference: TypeInference,
                     var initialm:Int = 0, var k:Int = 3, var q:Int = 4,
                     var force_depth_count_1:Boolean = false,
                     var maxTry: Int = 5, var initialHashSeed:Int = 0, val caseSensitive: Boolean = false)
{
  var Q:Int = _
  var hashSeed:Int = _
  var n:Int = _
  var m = initialm // initially m is set to the given value, it will be updated when m = 0
  var non_zero_n : Int = _

  // objects that defines Bloomier filter
  var table: Table = _
  var header: Header = _
  var hasher: BloomierHasher = _

  var byteArrayBloomierFilter : ByteArrayBloomierFilter = _

  var inputByteArray: Map[JString, Array[SByte]] = null
  if (inputAny != null) {
    inputByteArray = BloomierFilter.mapConversion(inputAny = inputAny, typeInference = typeInference, q = q)
  }
  byteArrayBloomierFilter = new ByteArrayBloomierFilter(input = inputByteArray,
      initialm = initialm, k = k, q = q,
      force_depth_count_1 = force_depth_count_1,
      maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)

  BloomierFilter.copyParameters(from = byteArrayBloomierFilter, to = this)

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
    * @param sizeInBytes
    * @return
    */
  def getFullByteArray(label:JString, sizeInBytes:Int = 0) : Array[SByte] = {
    var totalSize = sizeInBytes
    if (sizeInBytes == 0) { // This is the string, so we need to figure out the size
      val row = byteArrayBloomierFilter.getByteArray(label).get
      totalSize = row(0)
    }
    val (count, remainder) = (totalSize / Q, totalSize % Q)
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
      val numberedKey:JString = if (index == 0) label else s"${label}${index}"
      byteArray ++= byteArrayBloomierFilter.getByteArray(numberedKey).get
    }

    byteArray
  }

  def get(label:JString) : Option[Any] = {
    val t = typeInference.get(label)
    if (t.isEmpty) None
    else {
      val v = t.get
      val sizeInBytes = v.sizeInBytes
      val ba = getFullByteArray(label, sizeInBytes)
      v.decode(ba)
    }
  }

  def serialize = {
    byteArrayBloomierFilter.serialize
  }

  def save(filePath:JString, ba:Array[SByte] = null) = {
    var byteArray = ba
    if (byteArray == null)
      byteArray = serialize
    byteArrayBloomierFilter.save(filePath, byteArray)
  }
  def load(filePath:JString) = {
    byteArrayBloomierFilter.load(filePath)
    BloomierFilter.copyParameters(from = byteArrayBloomierFilter, to = this)
  }
}
