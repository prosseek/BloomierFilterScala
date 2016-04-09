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
}
