package bloomierfilter.conversion

import util.types.TypeInference

import java.lang.{String => JString}
import scala.{Byte => SByte}
import scala.collection.mutable.{Map => MMap}

class Encoder (var typeInference: TypeInference) {

  def encode(input:Map[JString, Any]): Map[JString, Array[Byte]] = {
    val result = MMap[JString, Array[Byte]]()

    input foreach {
      case (key, value) => result(key) = encode(key, value)
    }

    result.toMap
  }

  def encode[T](input:JString, value:Any)() : Array[SByte] = {
    // 1. check if input is known type
    val res1 = typeInference.encodeFromLabel(input, value)
    if (res1.isDefined) {
      res1.get
    }
    // 2. check if the value is a byte/short/int/float/string
    else {
      value match {
        // any int/short/byte is transformed into 1 byte bytearray
        case v:Byte => util.conversion.ByteArrayTool.byteToByteArray(v.asInstanceOf[Byte])
        case v:Short => util.conversion.ByteArrayTool.shortToByteArray(v.asInstanceOf[Short])
        case v:Int => util.conversion.ByteArrayTool.intToByteArray(v.asInstanceOf[Int])
        case v:Double => util.conversion.ByteArrayTool.floatToByteArray(v.asInstanceOf[Float])
        case v:JString => util.conversion.ByteArrayTool.stringToByteArray(v)
        case _ => throw new RuntimeException(s"Error: type ${input}")
      }
    }

  }
}
