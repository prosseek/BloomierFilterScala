package bloomierfilter.conversion

import scala.collection.mutable.{Map => MMap}
object Util {
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
  def zeroPatchByteArray(key:String, value:Array[Byte], Q:Int) : Map[String, Array[Byte]] = {
    val result = MMap[String, Array[Byte]]()
    val size = value.size
    if (size == Q) result += (key -> value)
    else if (size < Q) { // patching
      result += (key -> util.conversion.ByteArrayTool.zeroPatch(byteArray = value, goalSize = Q))
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
        val finalByteArray = util.conversion.ByteArrayTool.zeroPatch(byteArray = finalSlice, goalSize = Q)
        result += (keyName -> finalByteArray)
      }
    }

    result.toMap
  }
}
