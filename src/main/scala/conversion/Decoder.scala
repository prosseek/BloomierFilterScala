package conversion

import util.types.TypeInference

import java.lang.{String => JString}
import scala.{Byte => SByte}

class Decoder (var typeInference: TypeInference) {
  def decode(key:String, value: Array[Byte], size:Int) : Option[Any] = ???
}
