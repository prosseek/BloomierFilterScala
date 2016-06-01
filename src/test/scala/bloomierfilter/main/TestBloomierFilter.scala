package bloomierfilter.main

import chitchat.typefactory.TypeDatabase
import org.scalatest.FunSuite
import java.lang.{String => JString}

class TestBloomierFilter extends FunSuite {

  def save(n:Int, cbf:Boolean=false) = {
    val ti = TypeDatabase()
    val m = Map[JString, Any]("age" -> 10, "string" -> "James")
    val bf = new BloomierFilter(inputAny = m, q = 8*n, typeDatabase = ti, force_m_multiple_by_four = true, force_depth_count_1 = cbf)
    bf.save(s"./src/test/resources/test_Q_${n}_cbf_${cbf}.bin")
    assert(bf.get("string").get == "James")
    assert(bf.get("age").get == 10)
    assert(bf.get("What").isEmpty)
  }
  def load(n:Int, cbf:Boolean=false) {
    val ti = TypeDatabase()
    val ba = null
    val bf = BloomierFilter(typeDatabase = ti, filePath = s"./src/test/resources/test_Q_${n}_cbf_${cbf}.bin")
    assert(bf.get("string").get == "James")
    assert(bf.get("age").get == 10)
    assert(bf.get("What").isEmpty)
  }
  def iterationTest(end:Int, cbf:Boolean) = {
    for (i <- 1 to end) {
      val ti = TypeDatabase()
      //val m = Map[JString, Any]("age" -> 10, "string" -> "James", "latitude" -> Seq(1,1,1,1))
      val m = Map[JString, Any]("age" -> 10, "string" -> "James")
      try {
        val bf = new BloomierFilter(inputAny = m, q = 8 * i, typeDatabase = ti, force_depth_count_1=cbf, force_m_multiple_by_four = true)

        assert(bf.get("string").get == "James")
        assert(bf.get("age").get == 10)

        if (List(1,2,4,8).toSet.contains(i)) {
          bf.save(s"./src/test/resources/itertest_Q_${i}_${cbf}.bin")
        }
      }
      catch {
        case e:Exception => {
          println(s"ERROR at ${i}")
          throw new Exception(s"ERROR at ${i}")
        }
      }
    }
  }

  test("simple when m is given") {
    iterationTest(end=10, cbf=false)
  }

  test("simple when m is given cbf true") {
    iterationTest(end=10, cbf=true)
  }

  test("save/load 1 - 8") {
    List(1,2,4,8) foreach { i =>
      save(i)
      load(i)
    }
  }

  test("save/load 1 - 8 CBF = true") {
    List(1,2,4,8) foreach { i =>
      save(i, cbf = true)
      load(i, cbf = true)
    }
  }

  test("bloomier filter encoded with the values not in the database") {
    val ti = TypeDatabase()
    val m = Map[JString, Any]("x" -> "hello", "string" -> "James", "boo" -> 12.8f)
    val bf = new BloomierFilter(inputAny = m, q = 8*2, typeDatabase = ti, force_m_multiple_by_four = true)
    assert(bf.get("string").get == "James")
    assert(bf.get("x").get == "hello")
    assert(bf.get("What").isEmpty)
    assert(bf.get("boo").isEmpty)
  }

  test("information") {
    val ti = TypeDatabase()
    val m = Map[JString, Any]("x" -> "hello", "string" -> "James", "boo" -> 12.8f)
    val bf = new BloomierFilter(inputAny = m, q = 8 * 2, typeDatabase = ti, force_m_multiple_by_four = true)
    println(bf.information)
  }
}
