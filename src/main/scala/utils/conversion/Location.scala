package utils.conversion

import scala.collection.BitSet
import utils.conversion.ByteArray._

/**
 * Created by smcho on 6/4/14.
 */

object Location {
  val ADDEDBITS = (8 + 3)
}

class Location () {
  var l: Latitude = null
  var a: Altitude = null

  def setLA(l:Latitude, a:Altitude) = {
    this.l = l
    this.a = a
  }

  def this(l:Latitude, a:Altitude) = {
    this()
    setLA(l, a)
  }

  def this(ba:Array[Byte]) = {
    this()
    val sa = byteArrayToBitSet(ba)
    if (utils.crc.CRC.decode(sa, addedBits = Location.ADDEDBITS)) {
      val (parity,pre_data) = sa.partition(_ < Location.ADDEDBITS)
      val data = pre_data.map(_ - Location.ADDEDBITS)(BitSet.canBuildFrom)
      val (lat,pre_alt) = data.partition(_ < PartialLocation.LATITUDE + 6*3)
      val alt = pre_alt.map(_ - (PartialLocation.LATITUDE + 6*3))(BitSet.canBuildFrom)

      val ll = new Latitude(lat, t=PartialLocation.LATITUDE)
      val aa = new Altitude(alt, t=PartialLocation.ALTITUDE)
      setLA(ll, aa)

//      println(lat)
//      println(alt)
    } else {
      throw new Exception("WRONG CRC CHECK")
    }
  }


  def toBitSet() = {
    val bsl = l.toBitSet
    // altitude comes *after* latitude
    val asl = a.toBitSet.map(_ + (PartialLocation.LATITUDE + 6*3))(BitSet.canBuildFrom)
    utils.crc.CRC.encode(bsl ++ asl, addedBits = 8+3)
  }
  def toByteArray() = {
    bitSetToByteArray(toBitSet())
  }

  def getL() = this.l.getDms()
  def getA() = this.a.getDms()
}
