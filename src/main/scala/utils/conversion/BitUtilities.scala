package utils.conversion

import collection.mutable.BitSet

/**
 * Created by smcho on 6/3/14.
 */
object BitUtilities {
  def byteToBitSet(x:Byte) = {
    val unsigned = x & 0xFF
    val res = BitSet()
    for (i <- 0 to 7) if (((unsigned >> i) & 1) == 1) res += i
    res
  }
}
