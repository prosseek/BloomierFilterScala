package core

/**
 * Created by smcho on 6/1/14.
 */

case class CustomException(val smth:String) extends Exception(smth)
case class OrderAndMatch(val hashSeed:Int, val piList:List[String], val tauList:List[Int])

class OrderAndMatchFinder(val keysDict:scala.collection.immutable.Map[String, Any],
                          val m:Int, val k:Int, val q:Int, val maxTry:Int = 5, val initialHashSeed:Int = 0) {
  var piList = List[String]()
  var tauList = List[Int]()
  var hashSeed = 0

  /**
   * This is for debugging purposes
   * @param key
   */
  def getNeighbors(key:String) = {
    utils.hash.Hash.getUniqueHashes(key = key, count = k, maxVal = m, startSeed = hashSeed)
  }

  def findMatch(remainingKeysDict:scala.collection.mutable.Map[String, Any],
                hasher:BloomierHasher): Boolean = {

    // when there is no dictionary for Pi and Tau left, it means everything is ordered
    if (remainingKeysDict.size == 0) return true

    var piTemp = scala.collection.mutable.ListBuffer[String]()
    var tauTemp = scala.collection.mutable.ListBuffer[Int]()

    val tweaker = new SingletonFindingTweaker(remainingKeysDict.toMap, hasher)

    // find
    for (k <- remainingKeysDict.keys) {
      // find the key has k that is not occupied by others
      val res = tweaker.tweak(k)
      if (res != -1) {
        piTemp += k
        tauTemp += res
      }
    }

    // none of the keys in remainingKeys have empty spot, so return false and try again
    if (piTemp.size == 0) return false

    // remove all piTemp members from the remaining key and try again.
    for (pi <- piTemp) remainingKeysDict -= pi

    if (remainingKeysDict.size != 0) {
      if (findMatch(remainingKeysDict, hasher) == false)  return false
    }

    this.piList ++= piTemp
    this.tauList ++= tauTemp

    return true;
  }

  def find() : OrderAndMatch = {
    var remainKeys = collection.mutable.Map(this.keysDict.toSeq: _*)
    val hashSeedList = scala.collection.mutable.ListBuffer[Int]()
    for (i <- 0 until maxTry) {
      var newHashSeed = initialHashSeed * i * 100
      hashSeedList += newHashSeed
      val h = BloomierHasher(hashSeed = newHashSeed, m = m, k = k, q = q)
      if (findMatch(remainKeys, h)) {
        this.hashSeed = newHashSeed
        return OrderAndMatch(hashSeed = newHashSeed, this.piList, this.tauList)
      } else {
        remainKeys = collection.mutable.Map(this.keysDict.toSeq: _*)
      }
    }
    throw new CustomException("Over max attempt")
  }
}
