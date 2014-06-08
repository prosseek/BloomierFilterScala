package core

/**
 * Created by smcho on 6/2/14.
 */
abstract class Decoder {
  def decode(key:String, data: Array[Byte], size:Int) : Option[Any]
}

//def decode(key:String, data: Array[Byte], size:Int) = {
//// select only size
//val res = utils.conversion.ByteArray.byteArrayToInt(data)
//Some(res)
//}