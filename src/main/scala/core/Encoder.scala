package core

/**
 * Created by smcho on 6/2/14.
 */

object Encoder {
  val altitudeStart = 0.0F
  val altitudeEnd = 90.0F
  val altitudeShift = 180.0F
  val latitudeStart = 0.0F
  val latitudeEnd = 90.0F
  val latitudeShift = 180.0F
  val tempStart = -100.0F
  val tempEnd = 200.0F
  val tempShift = 100.0F

  val altitude = "ALTITUDE"
  val latitude = "LATITUDE"
  val temperature = "TEMPERATURE"
  val time = "TIME"
  val date = "DATE"

  val yearLimit = 5000

  val enumLimit = 256
}

abstract class Encoder {
  //def encode(key:String, value: Any, size:Int = 0) : (String, Array[Byte])
  def encode(key:String, value: Any, size:Int = 0) : Array[Byte]
}

