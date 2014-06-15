
/**
 * Created by smcho on 6/11/14.
 */
import utils.conversion.ByteArray._

import scala.collection.mutable


class FPexplorer (totalByte :Int = 4) {
  var sign: Int = 1
  val expo = mutable.BitSet()
  val frac = mutable.BitSet()
  var expoSum : Int = 0
  var fSum : Int = 0
  var fracSum : Double = 0
  var result : Double = 0

  val expoBits = if (totalByte == 4) 8 else 11
  val startOfExpoBit = (totalByte*8 - 1 - expoBits)
  val endOfExpoBit = (totalByte*8 - 1)
  val fractionBits = if (totalByte == 4) 23 else 52

  def getRandomBitSet(size:Int) = {
    val r = scala.collection.mutable.BitSet()
    r.clear()
    for(i <- 0 until size) {
      if (scala.util.Random.nextBoolean())
        r += i
    }
    //println(r.mkString("*"))
    scala.collection.immutable.BitSet(r.toSeq: _*)
  }

  def printInfo() = {
    println(s"exp:${expo.mkString(":")} = ${expoSum}")
    println(s"frac:${frac.mkString(":")} = ${fracSum}(${fSum})")
    println(s"result:${result}")
  }

  def bsToFloat(bs:scala.collection.immutable.BitSet) = {
    sign = 1
    expo.clear()
    frac.clear()

    for (i <- bs) {
      //println(i)
      i match {
        case 31 => sign = -1

        case i if startOfExpoBit until endOfExpoBit contains i =>
          expo += (i - startOfExpoBit)
        case j => frac += i
      }
    }
    expoSum = (0 /: expo) {(acc, v) => acc + (1 << v)}
    fSum = (0 /: frac) {(acc, v) => acc + (1 << v)}
    // 0 bit -> 2^(-fractionBits)
    // 22 bit -> 1/2
    fracSum = (1.0 /: frac) {(acc, v) => acc + scala.math.pow(2.0, -(fractionBits - v))}
    result = sign*scala.math.pow(2.0, (expoSum.toDouble - 127.0))*fracSum
    result
  }

  def unpack(x: Float) = {
    def reverse(i : Int) = {
      val index = i % 8
      val byteIndex = (totalByte - 1) - (i / 8) // BigEndianness: 0 -> 3, 1 -> 2, 3 -> 0
      8 * byteIndex + index
    }

    val b = byteArrayToBitSet(intToByteArray(java.lang.Float.floatToIntBits(x)))
    // 1 to 10 -> (1..10)
    val startOfExpoBit = (totalByte*8 - 1 - expoBits)
    val endOfExpoBit = (totalByte*8 - 1)
    val fractionBits = if (totalByte == 4) 23 else 52

    val bs = b.map(reverse(_))
    bsToFloat(bs)
  }
}

object FPexplorer extends App {
  var x = new FPexplorer()
//  x.unpack(0.0000001F)
//  x.printInfo()
//  x.unpack(0.00000001F)
//  x.printInfo()

  var maxValue = Float.MinValue
  var minValue = Float.MaxValue
  var count = 0
  val histo = mutable.Map[String, Int]() withDefaultValue(0)
  for (i <- 1 to 1000000) {
    var j = x.getRandomBitSet(32)

    x.bsToFloat(j)
    //println(x.printInfo())
    val result = x.result.toFloat
//    println(result)
    if (result > maxValue) maxValue = result
    if (result < minValue) minValue = result
    if (result > 223.0 && result < 1273.0) count += 1
    if (result >= 0.0 && result < 1.0) {
      histo("0.0-1.0") += 1
    }
    if (result >= 1.0 && result < 10.0) {
      histo("1.0-10.0") += 1
    }
    if (result >= 10.0 && result < 100.0) {
      histo("10.0-100.0") += 1
    }
    if (result >= 100.0 && result < 1000.0) {
      histo("100.0-1000.0") += 1
    }
    if (result >= 1000.0) {
      histo(">1000.0") += 1
    }
    if (result < 0.0 && result >= -1.0) {
      histo("-:0.0-1.0") += 1
    }
    if (result < -1.0 && result >= -10.0) {
      histo("-:1.0-10.0") += 1
    }
    if (result < -10.0 && result >= -100.0) {
      histo("-:10.0-100.0") += 1
    }
    if (result < -100.0 && result >= -1000.0) {
      histo("-:100.0-1000.0") += 1
    }
    if (result < -1000.0) {
      histo("<-1000.0") += 1
    }
  }
  println(s"MAX:${maxValue}, MIN:${minValue}, COUNT:${count}")
  println(histo)
  // randomly generate value
}