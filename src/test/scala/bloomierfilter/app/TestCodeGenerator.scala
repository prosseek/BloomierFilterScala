package bloomierfilter.app

import bloomierfilter.main.BloomierFilter
import chitchat.typefactory.TypeDatabase
import org.scalatest.FunSuite

class TestCodeGenerator extends FunSuite {
  test("codeGen") {
    val ti = TypeDatabase()
    val m = Map[java.lang.String, Any]("x" -> "hello", "string" -> "James", "boo" -> 12.8f)
    val bf = new BloomierFilter(inputAny = m, q = 8 * 2, typeDatabase = ti, force_m_multiple_by_four = true)
    val res = CodeGenerator.codeGen(bf.information.toList.toMap)
    println(res)
  }
}
