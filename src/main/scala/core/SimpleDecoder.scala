package core

import utils.conversion.ByteArray

/**
 * Created by smcho on 6/8/14.
 */
class SimpleDecoder extends Decoder {
  def decode(key:String, value: Array[Byte], size:Int) = {
    key match  {
      case "Name"  => Some(ByteArray.byteArrayToString(value))
      case "Latitude" => Some(ByteArray.byteArrayToDouble(value))
      case "Altitude" => Some(ByteArray.byteArrayToDouble(value))
      case _ => throw new Exception("Not supported datatype")
    }
  }
}
