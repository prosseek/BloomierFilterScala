package core

/**
 * Created by smcho on 6/8/14.
 */
class DefaultEncoder extends Encoder {
  override def encode(key:String, value: Any, size:Int = 0) : Array[Byte] = {
    if (key == Encoder.altitude) {
      val encodeValue = value.asInstanceOf[Float]
      val x = if (encodeValue > 0) (encodeValue + Encoder.altitudeShift)
              else -(-encodeValue + Encoder.altitudeShift)
      utils.conversion.ByteArray.floatToByteArray(x, size)
    }
    else if (key == Encoder.latitude) {
      val encodeValue = value.asInstanceOf[Float]
      val x = if (encodeValue > 0) (encodeValue + Encoder.latitudeShift)
      else -(-encodeValue + Encoder.latitudeShift)
      utils.conversion.ByteArray.floatToByteArray(x, size)
    }
    else if (key == Encoder.temperature) {
      val encodeValue = value.asInstanceOf[Float]
      val x = encodeValue + Encoder.tempShift
      utils.conversion.ByteArray.floatToByteArray(x, size)
    }
    else if (value.isInstanceOf[Int])
      utils.conversion.ByteArray.intToByteArray(value.asInstanceOf[Int], size)
    //map(key) = toInt(value).get
    else if (value.isInstanceOf[Double])
      utils.conversion.ByteArray.doubleToByteArray(value.asInstanceOf[Double], size)
    else if (value.isInstanceOf[Float])
      utils.conversion.ByteArray.floatToByteArray(value.asInstanceOf[Float], size)
    else if (value.isInstanceOf[String])
      utils.conversion.ByteArray.stringToByteArray(value.asInstanceOf[String], size)  //value.asInstanceOf[String], size)
    else
      throw new Exception("WRONG INPUT!")
  }
}
