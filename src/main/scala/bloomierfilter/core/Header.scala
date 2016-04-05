package bloomierfilter.core

import chitchat.types.{Encoding, Range}

/*
 #### Header (2 bytes)
 * m (0 - 255) => (8 unsigned bits)
 * Q (1,2,4,8 only) => 2 bits
 * hashValue (0 - 63) => 6 bits
 */

class HeaderBits extends Encoding(
  name = "headerbits",
  Array[Range](
    new Range(name = "m",           size = 8, min = 0, max = 255, signed = false),
    new Range(name = "Q",           size = 2, min =   0, max = 3, signed = false),
    new Range(name = "hashValue",   size = 6, min =   0, max = 63, signed = false)))

/**
  * Created by smcho on 4/1/16.
  */

object Header {
  def apply(m:Int, Q:Int, hashSeed:Int) = {
    val h = new Header
    val byteArray = h.serialize(m, Q, hashSeed)
    h.byteArray = byteArray
    h
  }

  def apply(byteArray:Array[Byte]) = {
    val h = new Header
    val (m, qq, hashSeed) = h.decode(byteArray)
    h.m = m
    h.Q = qq
    h.hashSeed = hashSeed
    h
  }
}

/**
  * Header class owns m/Q/hashSeed value and its byte array representation
  * Header uses headerBits class to use its represestation and bloomierfilter.conversion.
  *
  */
class Header {

  val headerbits = new HeaderBits

  var byteArray: Array[Byte] = _
  var m: Int = _
  var Q: Int = _
  var hashSeed: Int = _
  val k: Int = 3

  def decode(bytearray: Array[Byte]) = {
    val result = headerbits.decode(bytearray.slice(0,2))
    if (result.isEmpty) throw new Exception(s"Header format error ${bytearray.mkString(":")}")
    else {
      val temp = result.get
      m = temp(0)
      Q = 1 << temp(1)
      hashSeed = temp(2)
    }
    (m, Q, hashSeed)
  }

  def serialize(m:Int, Q:Int, hashValue:Int) = {
    val map = Map[Int, Int](1 -> 0, 2 -> 1, 4 -> 2, 8 -> 3)
    if (!map.keySet.contains(Q))
      throw new RuntimeException(s"Only 1/2/4/8 is allowed in Q(${Q})")
    val qq = map(Q)
    headerbits.encode(Seq[Int](m, qq, hashValue))
  }

  def size = headerbits.sizeInBytes
}
