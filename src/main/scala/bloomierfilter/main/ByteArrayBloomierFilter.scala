package bloomierfilter.main

import bloomierfilter.core
import util.{Decoder, Encoder, Helper}

import scala.collection.mutable.ArrayBuffer

class ByteArrayBloomierFilter(val input:Map[String, Array[Byte]],
                              val m:Int, val k:Int, val q:Int,
                              val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = true) {
  val Q = util.conversion.Util.getBytesForBits(q)

  val n = input.size

  val keysDict = if (caseSensitive) input else Helper.createMapWithUppercaseKeys(input)
  val hasher = new core.BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)
  val oamf = new core.OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, maxTry = maxTry, initialHashSeed = initialHashSeed)
  val orderAndMatch = oamf.find()

  val table = Array.ofDim[Byte](m, Q)
  val hashSeed = orderAndMatch.hashSeed
  create

  def getByteArray(keyInput: String) : Option[Array[Byte]] = {
    val key = if (caseSensitive) keyInput else keyInput.toUpperCase()
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    if (Helper.checkAllZeroElementsInTable(neighbors, table)) {
      None
    } else {
      for (n <- neighbors) {
        valueToRestore = Helper.byteArrayXor(valueToRestore, table(n))
      }
      Some(valueToRestore)
    }
  }

  def create = {
    for ((key, i) <- orderAndMatch.piList.zipWithIndex) {
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      val mask = hasher.getM(key).toArray.map(_.toByte)
      val l = orderAndMatch.tauList(i)
      val L = neighbors(l) // L is the index in the table to store the value

      if (keysDict(key) != null) {
        val encodedValue = keysDict(key)
        if (encodedValue.size != Q)
          throw new RuntimeException(s"byte array size (${encodedValue.size}) is not the same as Q(${Q})")
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
    // we cannot use N because of CBF
    m - this.table.count(p => p.forall(_ == 0))
  }

  def size = {
    calculateN * Q
  }

  def serialize = ???
  def deserialize = ???
}
