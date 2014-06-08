package core

import utils.conversion.ByteArray

/**
 * Created by smcho on 6/8/14.
 */
class SimpleDecoder extends Decoder {
  def decode(key:String, value: Array[Byte], size:Int) = {
    key match  {
      case "Name"  => Some(ByteArray.byteArrayToString(value, size))
      case "Latitude" => Some(ByteArray.byteArrayToDouble(value, size))
      case "Altitude" => Some(ByteArray.byteArrayToDouble(value, size))
      case _ => throw new Exception("Not supported datatype")
    }
  }
}
