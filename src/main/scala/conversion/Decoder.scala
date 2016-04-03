package conversion

class Decoder {
  var typeInstances: Map[String, chitchat.types.Base[_]] = _

  def decode(key:String, value: Array[Byte], size:Int) : Option[Any] = ???
}
