package bloomierfilter.main

object FoldedByteArrayBloomierFilter {
  def apply() = ???
}

class FoldedByteArrayBloomierFilter(override val input:Map[String, Array[Byte]],
                                    override val initialm:Int = 0, override val k:Int = 3, override val q:Int,
                                    override val maxTry: Int = 5, override val initialHashSeed:Int = 0, override val caseSensitive: Boolean = false)
      extends ByteArrayBloomierFilter(input = input, initialm = initialm, k = k, q = q,
                                      maxTry = maxTry, initialHashSeed = initialHashSeed, caseSensitive = caseSensitive) {


  override def getByteArray(keyInput:String) = {
    super.getByteArray(keyInput)
  }
}
