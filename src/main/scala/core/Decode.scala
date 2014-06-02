package core

/**
 * Created by smcho on 6/2/14.
 */
object Decode {
  def decode(key:String, data: Array[Byte], size:Int) = {
    // select only size
    val res = utils.conversion.ByteArray.byteArrayToInt(data)
    Some(res)
  }
}
