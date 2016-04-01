package bloomierfilter.core

import util.Hash

class BloomierHasher(val m:Int = 100, val k:Int = 3, val q:Int = 32, val hashSeed:Int = 0) {
  val byteSize = util.conversion.Util.getBytesForBits(q)

  /** Randomly generate byteSize values (the value is between 0 - 255)
   *  M is the M in the original paper.
   *
   * @param key
   * @return
   */
  def getM(key: String, hashSeed:Int = hashSeed) = {
    Hash.getUniqueHashes(key = key, count = this.byteSize, maxVal = 256, startSeed = hashSeed)
  }

  /** Returns `k` unique (not duplicate) hash values from input string key
    *
    * @param key
    * @param hashSeed
    * @return
    */
  def getNeighborhood(key:String, hashSeed:Int = hashSeed) = {
    util.Hash.getUniqueHashes(key = key, count = this.k, maxVal = this.m, startSeed = hashSeed)
  }
}
