package core

/**
 * Created by smcho on 6/2/14.
 */
abstract class Decoder {
  def decode(key:String, data: Array[Byte], size:Int) : Option[Any]
}
