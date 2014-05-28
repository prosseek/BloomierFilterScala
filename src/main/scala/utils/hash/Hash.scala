package utils.hash

import scala.collection.mutable.SortedSet
import scala.util.hashing.MurmurHash3
case class CustomException(smth:String) extends Exception(smth)

object Hash {
  /**
   * Generates hash value between 0 and maxVal - 1
   * This is special because MurmurHash3 returns hash value from -(MaxVal - 1) to MaxVal -1
   *
   * @param name
   * @param maxVal
   * @param seed (default value = 0)
   * @return A unsigned integer value between (0 - (MaxVal-1))
   */
  def getUnsignedHash(name: String, maxVal: Int, seed: Int = 0) = ((MurmurHash3.arrayHash(name.toArray, seed) % maxVal) + maxVal - 1) / 2

  def getHashes(name: String, count: Int = 3, maxVal: Int = 100, startCount: Int = 0) =
    (for (i <- (0 + startCount) to (count - 1 + startCount)) yield getUnsignedHash(name, maxVal, i)).toList

  def getHashes2(name: String, count: Int = 3, maxVal: Int = 100) =
    (for (i <- 0 to count - 1) yield (MurmurHash3.arrayHash(name.toArray, i) % maxVal)).toList

  def getUniqueHashes(name: String, count: Int = 3, maxVal: Int = 100) = {
    if (count >= maxVal) throw new CustomException("Count should be smaller than maxVal")

    // when you can get the result with one attempt
    val set = getHashes(name, count, maxVal).to[SortedSet]
    var diff = count - set.size
    if (diff == 0) set.toList
    else {
      var new_seed = count
      while (diff > 0) {
        val newHashValue = getUnsignedHash(name, maxVal, new_seed)
        set += newHashValue

        diff = count - set.size
        new_seed += 1
        if (new_seed > maxVal * 2)
          throw new CustomException(s"Tried ${new_seed - count} times, but cannot find ${count} values, only ${set.size}")
      }
      set.toList
    }
  }

}