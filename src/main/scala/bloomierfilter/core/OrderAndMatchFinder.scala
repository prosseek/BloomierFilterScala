package bloomierfilter.core

import util.Hash

import scala.collection.mutable.{ListBuffer => LBuffer}
import scala.collection.mutable.{Map => MMap}

case class OrderAndMatch(val hashSeed:Int, val piList:List[String], val tauList:List[Int])

object OrderAndMatchFinder {
  // used in find() method
  var MAXIMUM_M : Int = 255
  // used in find().findOrderAndMatch() method
  //var FORCE_DEPTH_COUNT_1 : Boolean = false

  def apply(keys: Seq[String], initialM: Int, k: Int, maxTry: Int = 5, initialHashSeed: Int = 0, force_depth_count_1: Boolean = false) = {
    val om = new OrderAndMatchFinder(keys = keys, initialM = initialM, k = k, maxTry = maxTry, initialHashSeed = initialHashSeed, force_depth_count_1 = force_depth_count_1)
    om.find()
    om
  }
}

/** Finds the piList, tauList, and hashSeed for given keys (a list of strings)
  *
  * @param keys
  * @param initialM
  * @param k
  * @param maxTry
  * @param initialHashSeed
  */
class OrderAndMatchFinder(val keys: Seq[String], val initialM: Int, val k: Int, val maxTry: Int = 5,
                          val initialHashSeed: Int = 0,
                          val force_depth_count_1:Boolean = false,
                          val force_m_multiple_by_four:Boolean = false) {
  //********** Public Properties **********
  /**
    * ==== Why var, not val ====
    *
    * These are class fields, and declared as `var` as they are keep updated and reset to find ordering
    */
  val piList = LBuffer[String]()
  val tauList = LBuffer[Int]()
  var hashSeed = initialHashSeed
  var m = initialM

  // For OBF implementation & debugging
  var depthCount:Int = 0
  val orderHistory = MMap[Int, Seq[String]]()
  var tryCount:Int = 0

  /**
   * This is set to package scope for unit testing.
   *
   * @param key
   */
  def getNeighbors(key:String) = {
    Hash.getUniqueHashes(key = key, count = k, maxVal = m, startSeed = hashSeed)
  }

  /**
    * Removes the contents of the fields to search again
    */
  private def reset = {
    this.piList.clear()
    this.tauList.clear()
    this.orderHistory.clear()
    this.depthCount = 0
  }

  private def findMatch(keys:Seq[String],
                hasher:BloomierHasher): Boolean = {

    // when there is no dictionary for Pi and Tau left, it means everything is ordered
    if (keys.size == 0) return true

    tryCount += 1

    val remainingKeys = LBuffer(keys:_*)
    val piTemp = LBuffer[String]()
    val tauTemp = LBuffer[Int]()
    val tweaker = new SingletonFindingTweaker(remainingKeys, hasher)

    // find
    for (k <- remainingKeys) {
      // find the key has k that is not occupied by others
      val res = tweaker.tweak(k)
      // not NONSINGLETON means that they found singular, so delete them.
      if (res != SingletonFindingTweaker.NONSINGLETON) { // when the result is **singleton**
        piTemp += k
        tauTemp += res
      }
    }

    // none of the keys in remainingKeys have empty spot, so return false and try again
    if (piTemp.size == 0) return false

    // remove all piTemp members from the remaining key and try again.
    for (pi <- piTemp) remainingKeys -= pi

    if (remainingKeys.size != 0) {
      if (findMatch(remainingKeys, hasher) == false)  return false
    }

    // The deepest level values come this first recursively.
    orderHistory(depthCount) = piTemp
    depthCount += 1

    this.piList ++= piTemp
    this.tauList ++= tauTemp

    return true;
  }

  /** Returns the ordering with given `m`
    *
    * ==== Why finalTry ====
    *  1. When m is 0, we need to find the m value to get the ordering.
    *  2. We increase the value m, but when it's over the limit, we try with the maxium m value.
    *  3. We turn finalTry bit on for this final try.
    *  4. If this is not a final try, we return null to try with larger m, if it is a final try, to raise an error.
    *
    * ==== How FORCE_DEPTH_COUNT_1 is used ====
    *  - With OBF, the ordering should have only 1 depth.
    *  - When FORCE_DEPTH_COUNT_1 is set, ordering becomes true only when depth is 1
    *
    *
    * @param m
    * @param finalTry
    * @return
    */
  private def findOrderAndMatch(m:Int, finalTry:Boolean = false) : OrderAndMatch = {
    def everyKeyIsSingleton = {
      depthCount == 1
    }
    // for checking how many times we tried

    // collection.mutable.Map(this.keys.toSeq: _*)
    val hashSeedList = scala.collection.mutable.ListBuffer[Int]()

    for (i <- 0 until maxTry) {
      var remainKeys = this.keys
      reset

      // when we cannot find the order and match with the iital hash value
      // we change the seed value and try again
      var newHashSeed = initialHashSeed + i
      hashSeedList += newHashSeed
      val h = new BloomierHasher(hashSeed = newHashSeed, m = m, k = k)

      if (findMatch(remainKeys, h)) {
        // For CBF, depth count one should be enforced.
        if (force_depth_count_1) {
          if (everyKeyIsSingleton) { // depthCount 1 means everything is a singleton
            this.hashSeed = newHashSeed
            return OrderAndMatch(hashSeed = newHashSeed, this.piList.toList, this.tauList.toList)
          }
          // if not keep iterating
        } else
          return OrderAndMatch(hashSeed = newHashSeed, this.piList.toList, this.tauList.toList)
      }
    }
    // after maxTry, we return null to try with larger m
    // with final try, we throw exception
    if (finalTry)
      throw new RuntimeException(s"Over max attempt ${maxTry}")
    else
      null
  }

  //********** API **********

  /** Returns the ordering
    *
    * This is a wrapper for the core function `findOrderAndMatch`
    *
    * ==== Why do we need wrapper/core approach ====
    * When m is given, we try to find the ordering with varying hashSeed values up to maxTry times.
    *
    * ==== Algorithm ====
    *  1. If initialM is more than 0, use the value as m, and finds the ordering by **modifying hashSeed**.
    *  2. If initialM is 0, we need to find the m to make the ordering by **setting and increasing m**.
    *      - It invokes findOrderAndMatch with the guessed
    *      - m up to `MAXIMUM_M`
    *
    * ==== Remember ====
    *  When success
    *    - it updates the value m.
    *    - it updates the hashSeed.
    *
    * @return
    */
  def find() : OrderAndMatch = {
    def make_m_multiply_by_for(m:Int) = {
      // m should be updated
      // m = 7, 7 % 4 = 3, 4-3=1, so +1 makes 8
      // likewise 5, 5 % 4 = 1, 4-1=3, so +3 makes 8
      if (!force_m_multiple_by_four) m
      else {
        if (m % 4 != 0) {
          m + (4 - (m % 4))
        }
        else
          m
      }
    }

    // ***************************
    // Start of the main algorithm
    // ***************************

    if (initialM > 0) { // when m is given, find the ordering with the given value
      findOrderAndMatch(make_m_multiply_by_for(initialM), finalTry = true)
    } else {
      // when m = 0, we try to find the minimum value that finds the findOrderAndMatch
      // the initial value is n (keysDict.size)*1.5
      var tryM = make_m_multiply_by_for((keys.size * 2.0 + 0.5).toInt)
      while (tryM < OrderAndMatchFinder.MAXIMUM_M) {
        for (i <- 0 until maxTry) {
          val res = findOrderAndMatch(tryM, finalTry = false)
          if (res != null) {
            this.m = tryM
            return res
          }
          else {
            tryM = make_m_multiply_by_for((tryM * 1.5 + 0.5).toInt)
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
