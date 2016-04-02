package util

import scala.collection.mutable.SortedSet
import scala.util.hashing.MurmurHash3

object Hash {
  /**
   * Generates hash value between 0 and maxVal - 1
   * MurmurHash3 returns hash value from -(MaxVal - 1) to MaxVal -1, so we need transformation.
   *
   * ==== Constraints ====
   *  - Precondition: name should not be empty string and maxVal should be more than 1
   *
   * @param name
   * @param maxVal
   * @param seed (default value = 0)
   * @return A unsigned integer value between (0 - (MaxVal-1))
   */
  def getHash(name: String, maxVal: Int, seed: Int = 0) = {
    def abs(value:Int) = if (value > 0) value else -value

    if (maxVal == 0) throw new RuntimeException("maxVal should not be 0")
    if (name.size == 0) throw new RuntimeException("name should not be null string")

    // if maxVal is 5, the range is -4 - 4
    // we can add 5 to make -1 to 9 and we add 1 to make it 1 to 10
    abs(MurmurHash3.stringHash(name, seed) % maxVal)
  }

  /**
   * Generate count (default 3) unsigned hash values.
   *
   * @param name
   * @param count
   * @param maxVal
   * @param startSeed
   * @return List of (count) hash values (unsigned 0 - (maxVal-1))
   */
  def getHashes(name: String, count: Int = 3, maxVal: Int = 100, startSeed: Int = 0) =
    (for (i <- startSeed to (count - 1 + startSeed)) yield getHash(name, maxVal, i)).toList

  /**
   * Generates a list of **count** hash values between (0 - (maxVal-1)) that do not have duplication.
   * It throws an error when
   *   1. **count** is more than **maxVal**: It's not possible more than maxVal (0 until maxVal) unduplicated values.
   *   2. It will try to get **count** unique values trying to get new hash values (maxVal*2 - count) more times.
   *      It will raise an error when it does not find unique **count** values after trying (maxVal*2 - count)
   *
   * @param key
   * @param count
   * @param maxVal
   * @return
   */
  def getUniqueHashes(key: String, count: Int = 3, maxVal: Int = 100, startSeed: Int = 0) = {
    if (count >= maxVal) throw new RuntimeException(s"Count (${count}) should be smaller than maxVal (${maxVal})")

    // when you can get the result with one attempt
    val set = getHashes(key, count, maxVal, startSeed).to[SortedSet]
    var diff = count - set.size
    if (diff == 0) set.toList
    else {
      var new_seed = count
      while (diff > 0) {
        val newHashValue = getHash(key, maxVal, new_seed)
        set += newHashValue

        diff = count - set.size
        new_seed += 1
        if (new_seed > maxVal * 2)
          throw new RuntimeException(s"Tried ${new_seed - count} times, but cannot find ${count} values, only ${set.size}")
      }
      set.toList
    }
  }
}