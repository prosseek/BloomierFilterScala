package utils.conversion

import java.nio.ByteBuffer
import scala.collection.BitSet
import collection.mutable.{Map, ArrayBuffer}

/**
 * Created by smcho on 5/31/14.
 *
 * http://stackoverflow.com/questions/2183240/java-integer-to-byte-array
 */
object ByteArray {
  /*
    from Data : T -> ByteArray
   */
  def intToByteArray(x: Int) = dataToByteArray(x)
  def byteToByteArray(x: Byte) = dataToByteArray(x)
  def dataToByteArray(x: Int) = {
    ByteBuffer.allocate(4).putInt(x).array()
  }
  def dataToByteArray(x: Long) = {
    ByteBuffer.allocate(8).putLong(x).array()
  }
  def dataToByteArray(x: Byte) = {
    ByteBuffer.allocate(1).put(x).array()
  }
  def doubleToByteArray(x: Double) = {
    val l = java.lang.Double.doubleToLongBits(x)
    dataToByteArray(l)
  }
  def floatToByteArray(x: Float) = {
    val l = java.lang.Float.floatToIntBits(x)
    dataToByteArray(l)
  }
  def stringToByteArray(x: String, n:Int = 0) = {
    val size = if (n <= 0) x.size else n
    ByteBuffer.allocate(size).put(x.slice(0, size).getBytes()).array()
  }
  def bitSetToByteArray(x:BitSet) = {
    val bits = Map[Int, Byte]().withDefaultValue(0)
    for (i <- x) {
      val bitLocation = i % 8
      val group = i / 8
      bits(group) = (bits(group) + (1 << bitLocation)).toByte
    }
    val byteArray = Array.fill[Byte](bits.keys.max + 1)(0)

    for ((k,v) <- bits) {
      byteArray(k) = v
    }
    byteArray
  }

  /*
    ByteArray to Data : T
   */
  def byteArrayToString(x: Array[Byte]) = {
    val loc = x.indexOf(0)
    if (-1 == loc)
      new String(x)
    else if (0 == loc)
      ""
    else
      new String(x.slice(0,loc))
  }
  def byteArrayToByte(x: Array[Byte]) = {
    ByteBuffer.wrap(x).get()
  }
  def byteArrayToInt(x: Array[Byte]) = {
    ByteBuffer.wrap(x).getInt
  }
  def byteArrayToLong(x: Array[Byte]) = {
    ByteBuffer.wrap(x).getLong
  }
  def byteArrayToDouble(x: Array[Byte]) = {
    ByteBuffer.wrap(x).getDouble
  }
  def byteArrayToFloat(x: Array[Byte]) = {
    ByteBuffer.wrap(x).getFloat
  }
  def byteArrayToBitSet(x:Array[Byte]) = {
    var res = ArrayBuffer[Int]()
    for ((v,i) <- x.zipWithIndex if v != 0) {
      res.appendAll(BitUtilities.byteToBitSet(v).toArray.map(_ + 8*i))
    }
    BitSet(res: _*)
  }
}

