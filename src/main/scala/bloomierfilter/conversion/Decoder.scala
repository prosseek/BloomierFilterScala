package bloomierfilter.conversion

//import java.lang.{String => JString}
//
//import scala.{Byte => SByte}
//import bloomierfilter.main.BloomierFilter
//
//import chitchat.types._
//import chitchat.typetool.TypeInference
//
//class Decoder (override val typeInference: TypeInference, val bf:BloomierFilter)
//  extends chitchat.conversion.Decoder(typeInference = typeInference) {
//
//  def decode(key:JString) : Option[Any] = {
//    val decodedFromKey = decodeFromKey(key)
//    if (decodedFromKey.isDefined) {
//      return decodedFromKey
//    }
//    null
//  }
//
//  def decodeFromKey(key:JString) : Option[Any] = {
//    val typeFromLabel = typeInference.getChitchatTypeFromLabel(key)
//
//    if (typeFromLabel.isDefined) {
//      val (valueType, instance) = typeFromLabel.get
//
//      valueType match {
//        case "float" => {
//          val floatInstance = instance.asInstanceOf[Float]
//          val size = floatInstance.sizeInBytes
//          val ba = getFullByteArray(key, size)
//          Some(util.conversion.ByteArrayTool.byteArrayToFloat(ba))
//        }
//        case "string" => {
//          val ba0 = bf.getByteArray(key).get
//          val size = ba0(0) + 1 // total size is +1 from the string length
//          val ba = getFullByteArray(key, size)
//          val result = util.conversion.ByteArrayTool.byteArrayToString(ba)
//          Some(result)
//        }
//        case "encoding" => {
//          val encodingInstance = instance.asInstanceOf[Encoding]
//          val size = encodingInstance.sizeInBytes
//          val ba = getFullByteArray(key, size)
//          encodingInstance.decode(ba)
//        }
//        case "range" => {
//          val rangeInstance = instance.asInstanceOf[Range]
//          val size = rangeInstance.sizeInBytes
//          val ba = getFullByteArray(key, size)
//          rangeInstance.decode(ba)
//        }
//        case _ => {
//          throw new RuntimeException(s"Unknown type ${valueType}")
//        }
//      }
//    }
//    else {
//      None
//    }
//  }
//

//}
