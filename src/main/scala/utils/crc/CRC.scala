package utils.crc

import collection.mutable.BitSet
/**
 * Created by smcho on 6/4/14.
 */
object CRC {
  def encode(input:collection.BitSet, addedBits:Int, gin:Option[BitSet] = None) = {
    var r = input.map(_ + addedBits)(collection.BitSet.canBuildFrom)
    var g = gin.getOrElse(BitSet(Range(0,addedBits+1): _*))
    val maxG = g.max
    while (r.size > 0 && r.max >= maxG) {
      val diff = r.max - g.max
      g = g.map(_ + diff)(BitSet.canBuildFrom)
      r = r ^ g
    }
    input.map(_ + addedBits)(collection.BitSet.canBuildFrom) ++ r
  }
  def decode(input:collection.BitSet, addedBits:Int, gin:Option[BitSet] = None) = {
    var r = new BitSet(input.toBitMask)
    var g = gin.getOrElse(BitSet(Range(0,addedBits+1): _*))
    val maxG = g.max
    while (r.size > 0 && r.max >= maxG) {
      val diff = r.max - g.max
      g = g.map(_ + diff)(BitSet.canBuildFrom)
      r = r ^ g
    }
    if (r.size == 0) true else false
  }
}
