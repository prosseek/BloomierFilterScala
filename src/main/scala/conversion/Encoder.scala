package conversion

/**
  * Created by smcho on 4/1/16.
  */
class Encoder {
  var typeInstances: Map[String, chitchat.types.Base[_]] = _

  def encode(input:Map[String, Any]) = ???
  def encode(key:String, value: Any, size:Int = 0) : Array[Byte] = ???
}
