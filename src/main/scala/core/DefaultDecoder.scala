package core

/**
 * Created by smcho on 6/8/14.
 */
class DefaultDecoder extends Decoder {
  override def decode(key: String, data: Array[Byte], size: Int) = {
    if (key == Encoder.altitude) {
      val res = utils.conversion.ByteArray.byteArrayToFloat(data)
      if (ValueValidator.validateAltitude(res)) {
        var returnValue = res.getOrElse(0.0F)
        returnValue = if (returnValue >= 0.0) returnValue - Encoder.altitudeShift else returnValue + Encoder.altitudeShift
        Some(returnValue)
      }
      else Some("Wrong altitude")
    }
    else if (key == Encoder.latitude) {
      val res = utils.conversion.ByteArray.byteArrayToFloat(data)
      if (ValueValidator.validateLatitude(res)) {
        var returnValue = res.getOrElse(0.0F)
        returnValue = if (returnValue >= 0.0) returnValue - Encoder.latitudeShift else returnValue + Encoder.latitudeShift
        Some(returnValue)
      }
      else Some("Wrong latitude")
    }
    else if (key == Encoder.temperature) {
      val res = utils.conversion.ByteArray.byteArrayToFloat(data)
      if (ValueValidator.validateTemperature(res)) {
        var returnValue = res.getOrElse(0.0F) - Encoder.tempShift
        Some(returnValue)
      }
      else Some("Wrong temperature")
    }
    else if (key == Encoder.date) {
      val res = utils.conversion.ByteArray.byteArrayToInt(data)
      if (ValueValidator.validateDate(res)) res // res is already Option type
      else Some("Wrong date")
    }
    else if (key == Encoder.time) {
      val res = utils.conversion.ByteArray.byteArrayToInt(data)
      if (ValueValidator.validateTime(res)) res
      else Some("Wrong time")
    }
    else if (key.endsWith(" E") || key.endsWith(" e")) {
      val res = utils.conversion.ByteArray.byteArrayToInt(data)
      if (ValueValidator.validateEnum(res)) res
      else Some("Wrong enumeration")
      //utils.conversion.ByteArray.byteArrayToInt(data)
    }
    else if (key.endsWith(" I") || key.endsWith(" i")) {
      val res = utils.conversion.ByteArray.byteArrayToInt(data)
      if (ValueValidator.validateInt(res)) res
      else Some("Wrong integer")
      //utils.conversion.ByteArray.byteArrayToInt(data)
    }
    else if (key.endsWith(" F") || key.endsWith(" f")) {
      val res = utils.conversion.ByteArray.byteArrayToFloat(data)
      if (ValueValidator.validateFloat(res)) res
      else Some("Wrong float")
    }
    else if (key.endsWith(" D") || key.endsWith(" d")) {
      val res = utils.conversion.ByteArray.byteArrayToDouble(data)
      if (ValueValidator.validateDouble(res)) res
      else Some("Wrong double")
    }
    else if (key.endsWith(" S") || key.endsWith(" s")) {
      val res = utils.conversion.ByteArray.byteArrayToString(data)
      if (ValueValidator.validateString(res)) Some(res)
      else Some("Wrong string")
    }
    else {
      val res = utils.conversion.ByteArray.byteArrayToString(data)
      if (ValueValidator.validateReserved(res)) Some(res)
      else Some("Wrong reserved string")
    }
  }
}
