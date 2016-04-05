package bloomierfilter.conversion

import java.lang.{String => JString}
import scala.{Byte => SByte}

import bloomierfilter.main.BloomierFilter

import util.types.TypeInference
import chitchat.types._

class Decoder (val typeInference: TypeInference, val bf:BloomierFilter) {

  def decode(key:JString) : Option[Any] = {
    val decodedFromKey = decodeFromKey(key)
    if (decodedFromKey.isDefined) {
      return decodedFromKey
    }

    null
  }

  def decodeFromKey(key:JString) : Option[Any] = {
    val typeFromLabel = typeInference.getChitchatTypeFromLabel(key)

    if (typeFromLabel.isDefined) {
      val (valueType, instance) = typeFromLabel.get

      valueType match {
        case "float" => {
          val floatInstance = instance.asInstanceOf[Float]
          val size = floatInstance.sizeInBytes
          val ba = getFullByteArray(key, size)
          val result = util.conversion.ByteArrayTool.byteArrayToFloat(ba)
          Some(result)
        }
        case "string" => {
          val ba0 = bf.getByteArray(key).get
          val size = ba0(0) + 1 // total size is +1 from the string length
          val ba = getFullByteArray(key, size)
          val result = util.conversion.ByteArrayTool.byteArrayToString(ba)
          Some(result)
        }
        case "encoding" => {
          val encodingInstance = instance.asInstanceOf[Encoding]
          val size = encodingInstance.sizeInBytes
          val ba = getFullByteArray(key, size)
          encodingInstance.decode(ba)
        }
        case "range" => {
          val rangeInstance = instance.asInstanceOf[Range]
          val size = rangeInstance.sizeInBytes
          val ba = getFullByteArray(key, size)
          rangeInstance.decode(ba)
        }
        case _ => {
          throw new RuntimeException(s"Unknown type ${valueType}")
        }
      }
    }
    else {
      None
    }
  }

  /**
    * Retrieve the partial bytearrays (size Q) up to given size bytes
    *
    * @param size
    * @return
    */
  def getFullByteArray(key:JString, size:Int) : Array[SByte] = {
    val Q = bf.Q
    val (count, remainder) = (size / Q, size % Q)
    val totalCount =
      if (count == 0) 1 // get only one time if Q > size
      else {
        if (remainder == 0) count else count + 1
      }

    // the index is from 0 until totalCount
    // for example, size = 10, Q = 3 => count = 3, remainder = 1, totalCount = 4
    // the index ranges from 0 until 4 (0,1,2,3)
    //
    // another example, size = 10, Q = 5 => count = 2, remainder = 0, totalCount = 2
    // the index range from 0 until 2 (0, 1)

    var byteArray = Array[SByte]()
    for (index <- 0 until totalCount) {
      val numberedKey:JString = if (index == 0) key else s"${key}${index}"
      byteArray ++= bf.getByteArray(numberedKey).get
    }

    byteArray
  }
}
