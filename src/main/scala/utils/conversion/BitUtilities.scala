package utils.conversion

import collection.mutable.BitSet

/**
 * Created by smcho on 6/3/14.
 */
object BitUtilities {
  def byteToBitSet(x:Byte, shift:Int = 0) = {
    // BitSet(Array: _*)
    BitSet((for (i <- 0 to 7 if (((x & 0xFF) >> i) & 1) == 1) yield (i + shift)): _*)
  }
  def bitSetToByte(b:collection.BitSet, sh:Int=0) = ((0 /: b) {(acc, input) => acc + (1 << (input - sh))}).toByte
}
