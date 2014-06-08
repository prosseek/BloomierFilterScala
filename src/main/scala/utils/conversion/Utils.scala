package utils.conversion

/**
 * Created by smcho on 6/2/14.
 */
object Utils {
  /**
   * Gvien two byteArrays return the xor one by one
   * @param a
   * @param b
   */
  def byteArrayXor(a:Array[Byte], b:Array[Byte]) = {
    if (a.size != b.size) {
      throw new Exception(s"Array size is not the same: ${a.size} != ${b.size}")
    }
    val newArray = new Array[Byte](a.size)
    var i = 0
    for (ba <- a) {
      newArray(i) = (ba ^ b(i)).toByte
      i += 1
    }
    newArray
  }
}
