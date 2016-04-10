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
    new Range(name = "Q",           size = 2, min = 0, max = 3, signed = false),
    new Range(name = "hashSeed",    size = 5, min = 0, max = 31, signed = false),
    new Range(name = "complete",    size = 1, min = 0, max = 1, signed = false)))

/**
  * Created by smcho on 4/1/16.
  */

object Header {
  def apply(m:Int, Q:Int, hashSeed:Int, complete:Int) = {
    val h = new Header(m = m, Q = Q, hashSeed = hashSeed, complete = complete)
    val byteArray = h.encode()
    h.byteArray = byteArray
    h
  }

  def apply(byteArray:Array[Byte]) = {
    val h = new Header // it will be overwritten
    val (m, qq, hashSeed, complete) = h.decode(byteArray)
    h.m = m
    h.Q = qq
    h.hashSeed = hashSeed
    h.complete = complete
    h
  }
}

/**
  * Header class owns m/Q/hashSeed value and its byte array representation
  * Header uses headerBits class to use its represestation and bloomierfilter.conversion.
  *
  */
class Header(var m:Int = 0, var Q:Int = 0, var hashSeed:Int = 0, var complete:Int = 0) {

  val headerbits = new HeaderBits

  var byteArray: Array[Byte] = _
  val k: Int = 3

  def decode(bytearray: Array[Byte]) = {
    val result = headerbits.decode(bytearray.slice(0,2))
    if (result.isEmpty) throw new Exception(s"Header format error ${bytearray.mkString(":")}")
    else {
      val temp = result.get
      m = temp(0) * 4
      Q = 1 << temp(1)
      hashSeed = temp(2)
      complete = temp(3)
    }
    (m, Q, hashSeed, complete)
  }

  def encode(m:Int = m, Q:Int = Q, hashSeed:Int = hashSeed, complete:Int = complete) = {
    val map = Map[Int, Int](1 -> 0, 2 -> 1, 4 -> 2, 8 -> 3)
    if (!map.keySet.contains(Q))
      throw new RuntimeException(s"Only 1/2/4/8 is allowed in Q(${Q})")
    val qq = map(Q)

    // The m is right shift by 2 (divide by 4)
    val (div, rem) = (m / 4, m % 4)
    val m2 = if (rem == 0) div else (div + 1)

    headerbits.encode(Seq[Int](m2, qq, hashSeed, complete)).get
  }

  def size = headerbits.sizeInBytes
}
