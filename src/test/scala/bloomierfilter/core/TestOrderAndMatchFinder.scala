package bloomierfilter.core

import org.scalatest._
import scala.collection.mutable.ListBuffer
/**
 * Created by smcho on 6/2/14.
 */
class TestOrderAndMatchFinder extends FunSuite {

  test ("OrderAndMatchFinder simple check") {
    val keyMap = Seq[String]("abc", "def", "abd")
    val m = 6
    val k = 3
    val o = OrderAndMatchFinder(keys = keyMap, initialM = m, k = k, maxTry = 5, initialHashSeed = 0)
    o.find()

    /*
      012345 : m
      *  * * : "abc" (0,3,5)
        ***  : "abd" (2,3,4)
        * ** : "def" (2,4,5)

      "abc" has singleton at 0, and order 0
      "abd" has singleton at 3, and order 1
      "def" has singleton at 5, and order 2

 */
    assert(o.getNeighbors("abc") == List(0,3,5)) // 0th -> row 0 is selected
    assert(o.getNeighbors("abd") == List(2,3,4)) // 1st -> row 1 is selected
    assert(o.getNeighbors("def") == List(2,4,5)) // 0th -> row 2 is selected

    // We need to track of the hashSeed to get the same results
    // assert(o.hashSeed == 0)
    // println(o.depthCount)  // 2
    // println(o.orderHistory) // Map(1 -> ListBuffer(abc), 0 -> ListBuffer(abd, def))

    assert(o.m == 6)
    assert(o.hashSeed == 0)
    assert(o.depthCount == 2)
    assert(o.orderHistory == Map(1 -> ListBuffer("abc"), 0 -> ListBuffer("def", "abd")))

    assert(o.piList.zip(o.tauList).toMap == Map("def" -> 2, "abd" -> 1, "abc" -> 0))
  }

  test ("OrderAndMatchFinder with FORCE_DEPTH_COUNT_1") {
    val keyMap = Seq[String]("abc", "def", "abd")
    val m = 20
    val k = 3
    val o = OrderAndMatchFinder(keys = keyMap, initialM = m, k = k, maxTry = 100, initialHashSeed = 0, force_depth_count_1 = true)

    assert(o.getNeighbors("abc") == List(1,6,11)) // 0th -> row 0 is selected
    assert(o.getNeighbors("abd") == List(0,1,4)) // 1st -> row 1 is selected
    assert(o.getNeighbors("def") == List(5,10,18)) // 0th -> row 2 is selected

    // We need to track of the hashSeed to get the same results
    assert(o.hashSeed == 0)
    assert(o.depthCount == 1)  // 2
    assert(o.tryCount == 1)
  }

  test ("OrderAndMatchFinder with FORCE_DEPTH_COUNT_1 when m = 0") {
    val keyMap = Seq[String]("abc", "def", "abd")
    val m = 0
    val k = 3
    val o = OrderAndMatchFinder(keys = keyMap, initialM = m, k = k, maxTry = 100, initialHashSeed = 0, force_depth_count_1 = true)

    // We need to track of the hashSeed to get the same results
    assert(o.m == 6)
    assert(o.hashSeed == 2)
    assert(o.depthCount == 1)
    assert(o.tryCount == 5)
  }
}
