package utils.crc

import collection.mutable.BitSet
/**
 * Created by smcho on 6/4/14.
 */
object CRC {
  private def crcCore(input:collection.BitSet, addedBits:Int, gin:Option[BitSet], encode:Boolean = true) = {
    // if it's encode (encode == true), addedBits should be shifted, if not, just add 0 (simple copy)
    var r = input.map(_ + (if (encode) addedBits else 0))(collection.BitSet.canBuildFrom)
    // if g is not given, it's just "1...1".
    var g = gin.getOrElse(BitSet(Range(0,addedBits+1): _*))
    val maxG = g.max
    while (r.size > 0 && r.max >= maxG) {
      val diff = r.max - g.max
      g = g.map(_ + diff)(BitSet.canBuildFrom)
      r = r ^ g
    }
    r
  }

  def encode(input:collection.BitSet, addedBits:Int, gin:Option[BitSet] = None) = {
    val r = crcCore(input, addedBits, gin, encode=true)
    input.map(_ + addedBits)(collection.BitSet.canBuildFrom) ++ r
  }

  def decode(input:collection.BitSet, addedBits:Int, gin:Option[BitSet] = None) = {
    val r = crcCore(input, addedBits, gin, encode=false)
    if (r.size == 0) true else false
  }
}
