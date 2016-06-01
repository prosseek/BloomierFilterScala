package bloomierfilter.main

import chitchat.typefactory.TypeDatabase
import org.scalatest.FunSuite

class TestBloomierFilterIntegerName extends FunSuite {

  test("simple integer value") {
    val ti = TypeDatabase()
    val n = 4
    val m = Map[java.lang.String, Any]("old_i" -> 10, "string" -> "James")
    val bf = new BloomierFilter(inputAny = m, q = 8*n, typeDatabase = ti, force_m_multiple_by_four = true, force_depth_count_1 = false)
    assert(bf.get("string").get == "James")
    assert(bf.get("old_i").get == 10)
    assert(bf.get("What").isEmpty)
  }
}
