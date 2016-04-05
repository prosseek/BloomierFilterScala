package bloomierfilter.conversion

import bloomierfilter.main.BloomierFilter
import org.scalatest.FunSuite

class TestDecoder extends FunSuite {

  test ("decoder with Q=2 & Q = 8") {
    val inputAny = Map("string" -> "hello", "age" -> 10, "float" -> 5.6f, "time" -> Seq[Int](12, 11))
    val bits = 8

//    var bf = new BloomierFilter(inputAny, q = bits * 2)
//    println(bf.serialize.mkString(":"))
//    println(bf.serialize.size)
//    println(bf.size)
//    println(bf.decoder.decode("string"))
//    println(bf.decoder.decode("age"))
//    println(bf.decoder.decode("float"))
//    println(bf.decoder.decode("time"))

    var bf = new BloomierFilter(inputAny, q = bits * 8)
    println(bf.serialize.mkString(":"))
    println(bf.serialize.size)
    println(bf.size)

//    println(bf.decoder.decode("string"))
//    println(bf.decoder.decode("age"))
//    println(bf.decoder.decode("float"))
    println(bf.decoder.decode("time"))
  }
}
