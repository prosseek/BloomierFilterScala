package bloomierfilter.main

import bloomierfilter.core
import util.{Decoder, Encoder, Helper}

object ByteArrayBloomierFilter {
  def setupParameters(byteArray:Array[Byte]) = {
    val m = 3
    val n = 2
    val k = 3
    val q = 3
    val hashSeed = 0
    val Q = util.conversion.Util.getBytesForBits(q)
    val table = new core.Table(m, Q)
    (m, k, q, Q, hashSeed, table, n)
  }

  def apply(byteArray: Array[Byte]):ByteArrayBloomierFilter = {
    val (m, k, q, qq, hashSeed, table, n) = setupParameters(byteArray)
    val babf = new ByteArrayBloomierFilter(null, initialm = m, k = k, q = q)

    babf.Q = qq
    babf.hashSeed = hashSeed
    babf.table = table
    babf.n = n

    babf
  }
}

/**
  *
  * ==== inital m parameter ====
  *
  * When this value is 0, we run the algorithm to find the m large enough to
  * store all the inputs in the table.
  *
  * @param input
  * @param initialm
  * @param k
  * @param q
  * @param maxTry
  * @param initialHashSeed
  * @param caseSensitive
  */
class ByteArrayBloomierFilter(val input:Map[String, Array[Byte]],
                              val initialm:Int, val k:Int, val q:Int,
                              var maxTry: Int = 5, var initialHashSeed:Int = 0, var caseSensitive: Boolean = false) {

  // parameters that defines Bloomier Filter
  var Q:Int = _
  var hashSeed:Int = _
  var table: core.Table = _
  var n:Int = _
  var m = initialm // initially m is set to the given value, it will be updated when m = 0

  // when BF is constructed from input map
  var hasher:core.BloomierHasher = _
  var orderAndMatch: core.OrderAndMatch = _
  var keysDict: Map[String, Array[Byte]] = _

  if (input != null) {
    keysDict = if (caseSensitive) input else Helper.createMapWithUppercaseKeys(input)
    hasher = new core.BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)
    val oamf = new core.OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, maxTry = maxTry, initialHashSeed = initialHashSeed)
    orderAndMatch = oamf.find()
    if (m == 0) {
      m = oamf.m
    }

    Q = util.conversion.Util.getBytesForBits(q)
    hashSeed = orderAndMatch.hashSeed
    table = new core.Table(m, Q)
    n = input.size

    create
  }

  def getByteArray(keyInput: String) : Option[Array[Byte]] = {
    val key = if (caseSensitive) keyInput else keyInput.toUpperCase()
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    if (Helper.checkAllZeroElementsInTable(neighbors, table.table)) {
      None
    } else {
      for (n <- neighbors) {
        valueToRestore = Helper.byteArrayXor(valueToRestore, table.table(n))
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
            valueToStore = Helper.byteArrayXor(valueToStore, table.table(n))
          }
        }
        table.table(L) = valueToStore
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
    m - table.table.count(p => p.forall(_ == 0))
  }

  def size = {
    calculateN * Q + util.conversion.Util.getBytesForBits(m)
  }

  def serialize = ???
  def deserialize = ???
}
