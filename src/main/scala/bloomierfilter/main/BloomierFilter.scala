package bloomierfilter.main

import bloomierfilter.core
import bloomierfilter.util.{Encoder, Decoder, Helper}

class BloomierFilter(val keysDictInput:Map[String, Any],
                     val m:Int, val k:Int, val q:Int,
                     val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = true) {
  val Q = util.conversion.Util.getBytesForBits(q)

  val n = keysDictInput.size

  val keysDict = if (caseSensitive) keysDictInput else Helper.createMapWithUppercaseKeys(keysDictInput)
  val hasher = new core.BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)
  val oamf = new core.OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, maxTry = maxTry, initialHashSeed = initialHashSeed)
  val orderAndMatch = oamf.find()

  var encoder: Encoder = new Encoder
  var decoder: Decoder = new Decoder

  val table = Array.ofDim[Byte](m, Q)
  val hashSeed = orderAndMatch.hashSeed
  create(keysDict, orderAndMatch)


  def get(keyInput: String) : Option[Any] = {
    val key = if (caseSensitive) keyInput else keyInput.toUpperCase()
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    // If all of the row data in the table is zero, it means it's garbage data
    // Let's not be so smart for a while.
    // This is the routine to get the BOTTOM
    if (Helper.checkAllZeroElementsInTable(neighbors, table)) {
      //None // Bottom calculation
      Some("Bottom")
    } else {
      for (n <- neighbors) {
        valueToRestore = Helper.byteArrayXor(valueToRestore, table(n))
      }
      //decoder.decode(key, valueToRestore, Q)
    }
    None // check this
  }

  def create(keysDict:Map[String, Any], orderAndMatch:core.OrderAndMatch) = {

    for ((key, i) <- orderAndMatch.piList.zipWithIndex) {
      //val value : Int = keysDict(key).asInstanceOf[Int]
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      val mask = hasher.getM(key).toArray.map(_.toByte)
      val l = orderAndMatch.tauList(i)
      val L = neighbors(l) // L is the index in the table to store the value

      // only check when the encoded value is null
      if (keysDict(key) != null) {
        val encodedValue = encoder.encode(key, keysDict(key), Q)
        var valueToStore = Helper.byteArrayXor(mask, encodedValue)

        for ((n, j) <- neighbors.zipWithIndex) {
          if (j != l) {
            valueToStore = Helper.byteArrayXor(valueToStore, table(n))
          }
        }
        table(L) = valueToStore
      }
    }
  }

  /**
    * Get the size of r (m that is not zero)
    *
    * @return
    */
  def calculateN = {
    val result = m - this.table.count(p => p.forall(_ == 0))
    if (result != n)
      throw new RuntimeException(s"calculated N ($result) is different from the total number of element $n")
    result
  }

  def size = {
    calculateN * Q
  }

  def serialize = ???
  def deserialize = ???
}
