package core

import utils.conversion.ByteArray
/**
 * Created by smcho on 6/8/14.
 */
class SimpleEncoder extends Encoder {
  def encode(key: String, value:Any, size:Int = 0) = {
    key match  {
      case "Name"  => ByteArray.stringToByteArray(value.asInstanceOf[String])
      case "Latitude" => ByteArray.doubleToByteArray(value.asInstanceOf[Double])
      case "Altitude" => ByteArray.doubleToByteArray(value.asInstanceOf[Double])
      case _ => throw new Exception("Not supported datatype")
    }
  }
}
