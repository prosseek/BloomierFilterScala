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

  // int
  def intToByteArray(x: Int) = ByteBuffer.allocate(4).putInt(x).array()
  def intToByteArray(x: Int, size: Int) : Array[Byte] = {
    val res = intToByteArray(x)
    adjust(value = res, originalSize = 4, goalSize = size, signExtension = true)
  }

  // long
  def longToByteArray(x: Long) = ByteBuffer.allocate(8).putLong(x).array()
  def longToByteArray(x: Long, size: Int) : Array[Byte] = {
    val res = longToByteArray(x)
    adjust(value = res, originalSize = 8, goalSize = size, signExtension = true)
  }

  // byte
  def byteToByteArray(x: Byte) = ByteBuffer.allocate(1).put(x).array()
  def byteToByteArray(x: Byte, size: Int) : Array[Byte] = {
    val res = byteToByteArray(x)
    adjust(value = res, originalSize = 1, goalSize = size, signExtension = true)
  }

  // double
  def doubleToByteArray(x: Double) = {
    val l = java.lang.Double.doubleToLongBits(x)
    longToByteArray(l)
  }
  def doubleToByteArray(x: Double, size:Int) = {
    if (size < 8) throw new Exception(s"Double data should be at least 8 bytes, but given ${size}")
    val l = java.lang.Double.doubleToLongBits(x)
    adjust(longToByteArray(l), originalSize = 8, goalSize = size)
  }

  // float
  def floatToByteArray(x: Float) = {
    val l = java.lang.Float.floatToIntBits(x)
    intToByteArray(l)
  }
  def floatToByteArray(x: Float, size:Int) = {
    if (size < 4) throw new Exception(s"Float data should be at least 4 bytes, but given ${size}")
    val l = java.lang.Float.floatToIntBits(x)
    adjust(intToByteArray(l), originalSize = 4, goalSize = size)
  }

  // string
  def stringToByteArray(x: String, n:Int) = {
    assert (n >= x.size)
    // When n is given a value more than 0, it makes room for storing all of the n bytes
    //val size = x.size // if (n <= 0) x.size else n
    //ByteBuffer.allocate(n).put(x.slice(0, size).getBytes()).array()
    ByteBuffer.allocate(n).put(x.slice(0, x.size).getBytes()).array()
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

  /**
   *
   * @param x
   * @param size - not used
   * @return
   */
  def byteArrayToString(x: Array[Byte]) = {
    // detect the location of 0
    // http://stackoverflow.com/questions/23976309/trimming-byte-array-when-converting-byte-array-to-string-in-java-scala
    new String( x.array.takeWhile(_ != 0), "ASCII" )
  }
  // byte

  def byteArrayToByte(x: Array[Byte]) = {
    val (header, value) = x.splitAt(x.size - 1)
    if (header.forall(_ == 0)) Some(ByteBuffer.wrap(value)) else None

    //val value = x.slice(size - 1, size)
    //ByteBuffer.wrap(value).get()
  }

  // int
//  def byteArrayToInt(x: Array[Byte]) = {
//    ByteBuffer.wrap(x).getInt
//  }
  def byteArrayToInt(x: Array[Byte]) = {
    val (header, value) = x.splitAt(x.size - 4)
    if (header.forall(_ == 0)) Some(ByteBuffer.wrap(value).getInt) else None
  }

  // long
//  def byteArrayToLong(x: Array[Byte]) = {
//    ByteBuffer.wrap(x).getLong
//  }
  def byteArrayToLong(x: Array[Byte]) = {
//    val value = x.slice(size - 8, size)
//    ByteBuffer.wrap(value).getLong
    val (header, value) = x.splitAt(x.size - 8)
    if (header.forall(_ == 0)) Some(ByteBuffer.wrap(value).getLong) else None
  }

  // double
//  def byteArrayToDouble(x: Array[Byte]) = {
//    ByteBuffer.wrap(x).getDouble
//  }
  def byteArrayToDouble(x: Array[Byte]) = {
//    val value = x.slice(size - 8, size)
//    ByteBuffer.wrap(value).getDouble
    val (header, value) = x.splitAt(x.size - 8)
    if (header.forall(_ == 0)) Some(ByteBuffer.wrap(value).getDouble) else None
  }

  // float
//  def byteArrayToFloat(x: Array[Byte]) = {
//    ByteBuffer.wrap(x).getFloat
//  }
  def byteArrayToFloat(x: Array[Byte]) = {
//    val value = x.slice(size - 4, size)
//    ByteBuffer.wrap(value).getFloat
    val (header, value) = x.splitAt(x.size - 4)
    if (header.forall(_ == 0)) Some(ByteBuffer.wrap(value).getFloat) else None
  }

  def byteArrayToBitSet(x:Array[Byte]) = {
    var res = ArrayBuffer[Int]()
    for ((v,i) <- x.zipWithIndex if v != 0) {
      res.appendAll(BitUtilities.byteToBitSet(v).toArray.map(_ + 8*i))
    }
    scala.collection.immutable.BitSet(res: _*)
  }
}

