package core

case class BloomierParameter(val m:Int, val k:Int, val q:Int, val hashSeed:Int)

case class BloomierHasher(val m:Int = 100, val k:Int = 3, val q:Int = 32, val hashSeed:Int = 0) {
  val byteSize = util.conversion.Util.getBytesForBits(q)

  def this(p:BloomierParameter) = {
    this(p.m, p.k, p.q, p.hashSeed)
  }

  /**
   * Just create one byteSize (8 * byteSize bits) of random 0s and 1s
   * The startSeed = hashSeed*123 is just to create random values
   *
   * @param key
   * @return
   */
  def getM(key: String, hashSeed:Int = 1) = {
    util.hash.Hash.getUniqueHashes(key = key, count = this.byteSize, maxVal = 256, startSeed = hashSeed*123)
  }

  def getNeighborhood(key:String, hashSeed:Int = hashSeed) = {
    util.hash.Hash.getUniqueHashes(key = key, count = this.k, maxVal = this.m, startSeed = hashSeed)
  }
}
