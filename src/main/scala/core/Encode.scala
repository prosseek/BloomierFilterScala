package core

/**
 * Created by smcho on 6/2/14.
 */
object Encode {
  def encode(key:String, i: Int, size:Int) = {
    // select only size
    val res = utils.conversion.ByteArray.intToByteArray(i)
    // when the size (q in bytes) is more than 4 (the size of integer)
    if (size == 4) res
    else if (size > 4) {
      var value:Byte = 0
      if (res(0) < 0) value = -1.toByte
      val head = Array.fill[Byte](size - 4)(value)
      head ++ res
    } else {
      // size < 4
      res.slice(4-size, 4)
    }
  }
}
