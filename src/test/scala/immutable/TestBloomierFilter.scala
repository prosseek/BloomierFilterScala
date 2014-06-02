package immutable

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestBloomierFilter extends FunSuite {
  test ("Simple bloomier filter") {
    val inputMap = Map[String, Int]("abc"->10, "def"->20, "abd"->30)
    val b = new BloomierFilter(inputMap, m = 6, k = 3, q = 8*4)
    println(b.get("abc"))
    println(b.get("def"))
    println(b.get("abd"))
  }
}
