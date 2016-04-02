package bloomierfilter.main

import util.{Decoder, Encoder}

class BloomierFilter(val input:Map[String, Any],
                     val initialm:Int, val k:Int, val q:Int,
                     val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = true)
{
  val bytearrayInput = Encoder.encode(input)
  val bytearrayBloomierFilter = new ByteArrayBloomierFilter(bytearrayInput,
    initialm = initialm, k = k, q = q,
    maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive)
  val Q = bytearrayBloomierFilter.Q

  def get(key:String) = {
    val result = bytearrayBloomierFilter.getByteArray(key)
    if (result.isEmpty) None
    else {
      Decoder.decode(key = key, value = result.get, size = Q)
    }
  }
}
