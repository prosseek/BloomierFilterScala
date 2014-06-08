package core

import utils.conversion.ByteArray
/**
 * Created by smcho on 6/8/14.
 */
class SimpleEncoder extends Encoder {
  def encode(key: String, keysDict:Map[String, Any], size:Int = 0) = {
    key match  {
      case "Name"  => ByteArray.stringToByteArray(keysDict(key).asInstanceOf[String], size)
      case "Latitude" => ByteArray.doubleToByteArray(keysDict(key).asInstanceOf[Double], size)
      case "Altitude" => ByteArray.doubleToByteArray(keysDict(key).asInstanceOf[Double], size)
      case _ => throw new Exception("Not supported datatype")
    }
  }
}
