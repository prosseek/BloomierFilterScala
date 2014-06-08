package core

/**
 * Created by smcho on 6/2/14.
 */

abstract class Encoder {
  def encode(key:String, keysDict:Map[String, Any], size:Int = 0) : Array[Byte]
}

