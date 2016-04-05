package bloomierfilter.main

import org.scalatest.FunSuite

/**
  * Created by smcho on 4/2/16.
  */
class TestBloomierFilter extends FunSuite {
  test("simple when m is given") {

    val inputAny = Map("string" -> "hello", "age" -> 10)
    val Q = 8

    var bf = new BloomierFilter(inputAny, q = Q*2)
    println(bf.serialize.mkString(":"))

    bf = new BloomierFilter(inputAny, q = Q*8)
    println(bf.serialize.mkString(":"))

    println(bf.decoder.decode("string"))

//    val input: Map[String, chitchat.types.Base[_]]("string" -> "Hello", "age" -> 54)
//
//    val Q = 8
//    val bf = new BloomierFilter(input, q = 8*Q)
  }
}
