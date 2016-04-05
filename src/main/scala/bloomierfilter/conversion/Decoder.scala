package bloomierfilter.conversion

import util.types.TypeInference
import java.lang.{String => JString}

import bloomierfilter.main.BloomierFilter

import scala.{Byte => SByte}

class Decoder (val typeInference: TypeInference, val bf:BloomierFilter) {

  def decode(key:String) : Option[Any] = {
    val typeFromLabel = typeInference.getChitchatTypeFromLabel(key)

    if (typeFromLabel.isDefined) {
      val (valueType, instance) = typeFromLabel.get

      valueType match {
        case "float" => {
          val ba = bf.getByteArray(key).get
          val result = util.conversion.ByteArrayTool.byteArrayToFloat(ba)
          Some(result)
        }
        case "string" => {
          val ba = bf.getByteArray(key).get
          val result = util.conversion.ByteArrayTool.byteArrayToString(ba)
          Some(result)
        }
        case "encoding" => {
          val ba = bf.getByteArray(key).get
          val result = instance.asInstanceOf[chitchat.types.Encoding].decode(ba)
          Some(result)
        }
        case "range" => {
          val ba = bf.getByteArray(key).get
          val result = instance.asInstanceOf[chitchat.types.Range].decode(ba)
          Some(result)
        }
        case _ => {
          throw new RuntimeException(s"Unknown type ${valueType}")
        }
      }
    }
    else {
      null
    }
  }
}
