package core

/**
 * Created by smcho on 6/8/14.
 */
class DefaultDecoder extends Decoder {
  override def decode(key: String, data: Array[Byte], size: Int) = {
    val res = utils.conversion.ByteArray.byteArrayToInt(data)
    Some(res)
  }
}
