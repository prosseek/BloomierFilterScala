package utils.conversion

import collection.BitSet
/**
 * Created by smcho on 6/4/14.
 */
class Latitude extends PartialLocation(t = PartialLocation.LATITUDE) {
  def this(d:Int, m:Int, s1:Int, s2:Int) = {
    this()
    setDmsDd(d,m,s1,s2)
  }
  def this(bitSet:BitSet, t:Int) = {
    this()
    setFromBitSet(bitSet, t)
  }
}