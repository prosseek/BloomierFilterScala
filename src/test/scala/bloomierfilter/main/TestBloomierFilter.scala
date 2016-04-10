package bloomierfilter.main

import chitchat.typefactory.TypeDatabase
import chitchat.typetool.TypeInference
import org.scalatest.FunSuite
import java.lang.{String => JString}
/**
  * Created by smcho on 4/2/16.
  */
class TestBloomierFilter extends FunSuite {
  test("simple when m is given") {

    val ti = TypeInference()
    val m = Map[JString, Any]("age" -> 10, "string" -> "James")
    val bf = new BloomierFilter(inputAny = m, q = 8*2, typeInference = ti)

    assert(bf.get("string").get == "James")
    assert(bf.get("age").get == 10)
  }

  test("save") {
    val ti = TypeInference()
    val m = Map[JString, Any]("age" -> 10, "string" -> "James")
    val bf = new BloomierFilter(inputAny = m, q = 8*2, typeInference = ti)
    bf.save("./src/test/resources/test.bin")
  }

  test ("load binary") {
    val ti = TypeInference()
    val ba = null
    val bf = BloomierFilter(typeInference = ti, filePath = "./src/test/resources/test.bin")
    assert(bf.get("string").get == "James")
    assert(bf.get("age").get == 10)
    assert(bf.get("What").isEmpty)
  }
}
