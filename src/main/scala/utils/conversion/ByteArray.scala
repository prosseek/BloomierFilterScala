package utils.conversion

import java.nio.ByteBuffer

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
  def dataToByteArray(x: Int) = {
    ByteBuffer.allocate(4).putInt(x).array()
  }
  def dataToByteArray(x: Long) = {
    ByteBuffer.allocate(8).putLong(x).array()
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
}

