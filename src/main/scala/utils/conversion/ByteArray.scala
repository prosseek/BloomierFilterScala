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

  /**
   * Add more bytes to the value:Array[Byte]
   *
   * @param value
   * @param originalSize
   * @param goalSize
   */
  def adjust(value:Array[Byte], originalSize:Int, goalSize:Int, signExtension:Boolean = false) : Array[Byte] = {
    if (goalSize == originalSize) return value // nothing to do when the goal size is the same as originalSize
    if (goalSize < originalSize) throw new Exception(s"Goal size (${goalSize}}) should be larger than original size (${originalSize}})")


    var v:Byte = 0
    if (signExtension) {
      // ByteBuffer uses BigEndian, so use the lower bytes to check the sign
      if (value(0) < 0) v = -1.toByte
    }
    val head = Array.fill[Byte](goalSize - originalSize)(v)
    head ++ value
  }

  /*
    from Data : T -> ByteArray
   */
  def intToByteArray(x: Int) = dataToByteArray(x)
  def intToByteArray(x: Int, size: Int) = {
    val res = dataToByteArray(x)
    adjust(value = res, originalSize = 4, goalSize = size, signExtension = true)
  }
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

