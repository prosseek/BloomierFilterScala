package utils.conversion

import org.scalatest._
import collection.mutable.BitSet

/**
 * Created by smcho on 6/3/14.
 */
class TestBitUtilities extends FunSuite {
  test ("byteToBitSet test") {
    assert(BitUtilities.byteToBitSet(-1) == BitSet(0,1,2,3,4,5,6,7))
    assert(BitUtilities.byteToBitSet(0) == BitSet())
    assert(BitUtilities.byteToBitSet(1) == BitSet(0))
  }
  test ("byteToBitSet with shift test") {
    assert(BitUtilities.byteToBitSet(-1, 8) == BitSet(8,9,10,11,12,13,14,15))
    assert(BitUtilities.byteToBitSet(0) == BitSet())
    assert(BitUtilities.byteToBitSet(1, 8) == BitSet(8))
  }
  test("bitSetToByte") {
    assert(BitUtilities.bitSetToByte(BitSet(0,1,2,3)) == 15)
    assert(BitUtilities.bitSetToByte(BitSet(0,1,2,3,4,5,6,7)) == -1)
  }
}
