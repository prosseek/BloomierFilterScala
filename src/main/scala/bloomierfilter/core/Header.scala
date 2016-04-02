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
    new Range(name = "m",  size = 8, min = 0, max = 255, signed = false),
    new Range(name = "Q",  size = 2, min =   0, max = 3, signed = false),
    new Range(name = "hashValue",   size = 6, min =   0, max = 63, signed = false)))

/**
  * Created by smcho on 4/1/16.
  */
class Header(bytearray: Array[Byte]) {

  val headerbits = new HeaderBits

  def size = 4

  val m:Int = ???
  val q:Int = ???
  val hashSeed:Int = ???
  //val (m:Int, qq:Int, hashSeed:Int) = decode
  val k:Int = 3

  def decode = {
    headerbits.decode(bytearray.slice(0,2)).get
  }

  def encode(m:Int, Q:Int, hashValue:Int) = {
    headerbits.encode(Seq[Int](m, Q, hashValue))
  }

  def serialize = {
    Array[Byte]()
  }
}
