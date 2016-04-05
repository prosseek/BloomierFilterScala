package bloomierfilter.conversion

import bloomierfilter.main.BloomierFilter
import org.scalatest.FunSuite

class TestDecoder extends FunSuite {

  test ("decoder with Q=2 & Q = 8") {
    val inputAny = Map("string" -> "hello", "age" -> 10, "float" -> 5.6f, "time" -> Seq[Int](12, 11))
    val bits = 8

    var bf = new BloomierFilter(inputAny, q = bits * 2)
    assert(bf.serialize.mkString(":") == "1:14:9:55:126:69:31:-9:-92:-34:16:-90:-58:75:20:-116:-5:-20")
    assert(bf.serialize.size == 18)
    assert(bf.size == 16)
    assert(bf.decoder.decode("string") == Some("hello"))
    assert(bf.decoder.decode("age") == Some(10))
    assert(bf.decoder.decode("float").mkString == "5.6")
    assert(bf.decoder.decode("time") == Some(List(12, 11)))

    bf = new BloomierFilter(inputAny, q = bits * 8)
    assert(bf.serialize.mkString(":") == "3:8:23:55:72:1:48:54:85:-125:-45:113:-47:72:-126:-78:-45:96:43:16:45:46:59:1:23:-78:-50:5:87:95:-124:-99:-82:-59:-32")
    assert(bf.serialize.size == 35)
    assert(bf.size == 33)

    assert(bf.decoder.decode("string") == Some("hello"))
    assert(bf.decoder.decode("age") == Some(10))
    assert(bf.decoder.decode("float").mkString == "5.6")
    assert(bf.decoder.decode("time") == Some(List(12, 11)))
  }
}
