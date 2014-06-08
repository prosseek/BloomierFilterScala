package core

/**
 * Created by smcho on 6/8/14.
 */
class DefaultEncoder extends Encoder {
  override def encode(key: String, keysDict:Map[String, Any], size:Int): Array[Byte] = {
    // default encoder format is always string -> Int
    utils.conversion.ByteArray.intToByteArray(keysDict(key).asInstanceOf[Int], size)
  }
}
