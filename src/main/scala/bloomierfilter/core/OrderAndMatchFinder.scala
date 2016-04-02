package bloomierfilter.core

import util.Hash

case class OrderAndMatch(val hashSeed:Int, val piList:List[String], val tauList:List[Int])

object OrderAndMatchFinder {
  val MAXIMUM_M : Int = 255
}

class OrderAndMatchFinder(val keysDict: Map[String, Any], var m: Int, val k: Int, val maxTry: Int = 5, val initialHashSeed: Int = 0) {
  var piList = List[String]()
  var tauList = List[Int]()
  var hashSeed = 0

  // for debugging purposes
  var depthCount = 0
  var orderHistory : scala.collection.mutable.Map[Int, Any] = null

  /**
   * This is for debugging purposes
 *
   * @param key
   */
  def getNeighbors(key:String) = {
    Hash.getUniqueHashes(key = key, count = k, maxVal = m, startSeed = hashSeed)
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
      // not -1 means that they found singular, so delete them.
      if (res != SingletonFindingTweaker.NONSINGLETON) { // when the result is **singleton**
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

    // The deepest level values come this first recursively.
    orderHistory(depthCount) = piTemp
    depthCount += 1

    this.piList ++= piTemp
    this.tauList ++= tauTemp

    return true;
  }

  /** Returns OrderAndMatch
    *
    * When `m = 0`, this function tries to find the minimum m that allows the ordering.
    * It tries with increasing m up to `MAXIMUM_M`.
    *
    * ==== Warning ====
    *  When success, it updates the value m if m was 0.
    *
    * @return
    */
  def find() : OrderAndMatch = {

    def findOrderAndMatch(m:Int, finalTry:Boolean = true) : OrderAndMatch = {
      var remainKeys = collection.mutable.Map(this.keysDict.toSeq: _*)
      val hashSeedList = scala.collection.mutable.ListBuffer[Int]()

      for (i <- 0 until maxTry) {
        // when we cannot find the order and match with the iital hash value
        // we change the seed value and try again
        var newHashSeed = initialHashSeed + i
        hashSeedList += newHashSeed
        val h = new BloomierHasher(hashSeed = newHashSeed, m = m, k = k)

        orderHistory = scala.collection.mutable.Map[Int, Any]()
        depthCount = 0

        if (findMatch(remainKeys, h)) {
          this.hashSeed = newHashSeed
          return OrderAndMatch(hashSeed = newHashSeed, this.piList, this.tauList)
        } else {
          remainKeys = collection.mutable.Map(this.keysDict.toSeq: _*)
        }
      }
      // after maxTry, we return null to try with larger m
      // with final try, we throw exception
      if (finalTry)
        throw new RuntimeException(s"Over max attempt ${maxTry}")
      else
        null
    }

    if (m > 0) { // when m is given, find the ordering with the given value
      findOrderAndMatch(m)
    } else {
      // when m = 0, we try to find the minimum value that finds the findOrderAndMatch
      // the initial value is n (keysDict.size)*1.5
      var tryM = (keysDict.size * 2.0 + 0.5).toInt
      while (tryM < OrderAndMatchFinder.MAXIMUM_M) {
        for (i <- 0 until maxTry) {
          val res = findOrderAndMatch(tryM)
          if (res != null) {
            this.m = tryM
            return res
          }
          else {
            tryM = (tryM*2.0 + 0.5).toInt
          }
        }
      }
      // last try with maximum value
      val res = findOrderAndMatch(OrderAndMatchFinder.MAXIMUM_M, finalTry = true)
      this.m = OrderAndMatchFinder.MAXIMUM_M // no error means success
      res
    }
  }
}
