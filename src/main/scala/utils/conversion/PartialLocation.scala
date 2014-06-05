package utils.conversion

import scala.collection.BitSet
import utils.conversion.BitUtilities._
import utils.conversion.ByteArray._
/**
 * Created by smcho on 6/2/14.
 */
object PartialLocation {
  // 9 + 8 + 6*4 = 51 bits
  val LATITUDE = 9 // +- 180 (360), so 8 bits are needed 2^9 = 512
  val ALTITUDE = 8 // +- 90 (180), so 7 bits are needed 2^8 = 256

  def dms2dd(d:Int, m:Int, s:Double) = {
    if (d > 0)
      d.toDouble + m.toDouble/60.0 + s/3600.0
    else
      -(-d.toDouble + m.toDouble/60.0 + s/3600.0)
  }
  def dms2dd(d:Int, m:Int, s:String) : Double = {
    dms2dd(d,m,s.toDouble)
  }
  def dms2dd(d:Int, m:Int, s1:Int, s2:Int) : Double = {
    val s = s"${s1}.${s2}"
    dms2dd(d,m,s.toDouble)
  }
  def dd2dms(data:Double) = {
    // http://en.wikipedia.org/wiki/Decimal_degrees
    val d = data.toInt
    val m = (math.abs(data)*60.0).toInt % 60
    val value = math.abs(data)*3600.0
    val s1 = value.toInt % 60 // 0.5 is added to get round value
    val s2 = ((value - value.toInt)*100 + 0.5).toInt
    (d, m, s1, s2)
  }
}

class PartialLocation(val t:Int) {
  var dd : Double = 0.0
  var d :Int = 0
  var m :Int = 0
  var s1 :Int = 0
  var s2 :Int = 0

  def setDmsDd(d:Int, m:Int, s1:Int, s2:Int) = {
    this.d = d
    this.m = m
    this.s1 = s1
    this.s2 = s2
    this.dd = PartialLocation.dms2dd(d,m,s1,s2)
  }

  def this(d:Int, m:Int, s1:Int, s2:Int, t:Int = PartialLocation.LATITUDE) = {
    this(t)
    setDmsDd(d,m,s1,s2)
  }
  def this(dd:Double, t:Int) = {
    this(t)
    this.dd = dd
    val (d, m, s1, s2) = PartialLocation.dd2dms(dd)
    this.d = d
    this.m = m
    this.s1 = s1
    this.s2 = s2
  }

  def setFromBitSet(bitSet:BitSet, t:Int) = {
    var dd = BitSet()
    var mm = BitSet()
    var ss1 = BitSet()
    var ss2 = BitSet()
    for (i <- bitSet) {
      if (i < t) {
        dd += i
      } else if (i < t + 6) {
        mm += i
      } else if (i < t + 6 + 6) {
        ss1 += i
      } else {
        ss2 += i
      }
    }
    this.d = bitSetToByte(dd, 0)
    this.m = bitSetToByte(mm, t)
    this.s1 = bitSetToByte(ss1, t + 6)
    this.s2 = bitSetToByte(ss2, t + 6 + 6)
    this.dd = PartialLocation.dms2dd(d,m,s1,s2)
  }

  def this(bitSet:BitSet, t:Int) = {
    this(t)
    setFromBitSet(bitSet, t)
  }

  def this(b:Array[Byte], t:Int = PartialLocation.LATITUDE) = {
    this(byteArrayToBitSet(b),t)
  }

  def getDms() = (this.d, this.m, this.s1, this.s2)

  def toBitSet() = {
    // translate from d/m/s1/s2 to byte array
    // degree -> +-90 (7 bit) +- 180 (8 bit)
    val dd = byteToBitSet(d.toByte, 0)
    val mm = byteToBitSet(m.toByte, t)
    // minute 60 -> 6 bit (64)
    val ss1 = byteToBitSet(s1.toByte, t + 6)
    val ss2 = byteToBitSet(s2.toByte, t + 6 + 6)
    dd ++ mm ++ ss1 ++ ss2
  }
  def toByteArray() = {
    bitSetToByteArray(toBitSet())
  }
}
