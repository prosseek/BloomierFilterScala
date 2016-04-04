package conversion

/**
  * Created by smcho on 4/1/16.
  */
class Encoder (var typeInstances: Map[String, chitchat.types.Base[_]]) {
  def encode(key:String, value: Any, size:Int = 0) : Array[Byte] = {
    // 1. if key is known type, encode the value with the type
    util.types.Util.
  }
}
