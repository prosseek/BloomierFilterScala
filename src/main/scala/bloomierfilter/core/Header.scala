package bloomierfilter.core

/**
  * Created by smcho on 4/1/16.
  */
class Header(bytearray: Array[Byte]) {
  def size = 4

  val m:Int = ???
  val k:Int = ???
  val q:Int = ???
  val hashSeed:Int = ???

  def serialize = {
    Array[Byte]()
  }
}
