package bloomierfilter.main

import bloomierfilter.core._

object ByteArrayBloomierFilter {
  val HEADER_SIZE = 2

  private def createMapWithUppercaseKeys(keysDictInput:Map[String, Array[Byte]]) = {
    val map = collection.mutable.Map[String, Array[Byte]]()
    for ((key,value) <- keysDictInput) {
      map(key.toUpperCase()) = value
    }

    collection.immutable.Map(map.toSeq: _*)
  }

  private def analyzeSerialized(byteArray:Array[Byte]) = {

    val header = new Header(byteArray.slice(0, HEADER_SIZE))
    val m = header.m
    val k = header.k
    val q = header.q
    val hashSeed = header.hashSeed
    val Q = util.conversion.Util.getBytesForBits(q)

    val table = new Table(m, Q)
    val hasher = new BloomierHasher(m = m, k = k, q = q, hashSeed = hashSeed)

    val n = table.n

    (m, k, q, Q, hashSeed, table, header, hasher, n)
  }

  def apply(byteArray: Array[Byte]):ByteArrayBloomierFilter = {
    val (m, k, q, qq, hashSeed, table, header, hasher, n) = analyzeSerialized(byteArray)
    val babf = new ByteArrayBloomierFilter(null, initialm = m, k = k, q = q)

    babf.Q = qq
    babf.hashSeed = hashSeed
    babf.n = n

    babf.table = table
    babf.header = header
    babf.hasher = hasher

    babf.non_zero_n = table.calculate_non_zero_n

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
  var n:Int = _
  var m = initialm // initially m is set to the given value, it will be updated when m = 0
  var non_zero_n : Int = _

  // objects that defines Bloomier filter
  var table: Table = _
  var header: Header = _
  var hasher: BloomierHasher = _

  if (input != null) {
    // 1. find the ordering of the keys
    //    1.1 we get `m` if m == 0 (user's request to calculate m)
    //    1.2 we get hashSeed that enables the ordering
    val keysDict = if (caseSensitive) input else ByteArrayBloomierFilter.createMapWithUppercaseKeys(input)
    val oamf = new OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, maxTry = maxTry, initialHashSeed = initialHashSeed)
    val orderAndMatch = oamf.find()
    hashSeed = orderAndMatch.hashSeed
    if (m == 0) {
      m = oamf.m
    }

    // 2. make the hasher with updated m and hashSeed
    hasher = new BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)

    // 3. set other parameters
    Q = util.conversion.Util.getBytesForBits(q)
    table = new Table(m, Q)
    n = input.size

    // 4. create and set the table
    createTable(orderAndMatch, keysDict)

    // 5. set non_zero_n parameters with created table
    non_zero_n = table.calculate_non_zero_n
  }



  private def createTable(orderAndMatch: OrderAndMatch, keysDict: Map[String, Array[Byte]]) = {
    for ((key, i) <- orderAndMatch.piList.zipWithIndex) {
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      val mask = hasher.getM(key).toArray.map(_.toByte)
      val l = orderAndMatch.tauList(i)
      val L = neighbors(l) // L is the index in the table to store the value

      if (keysDict(key) != null) {
        val encodedValue = keysDict(key)
        if (encodedValue.size != Q)
          throw new RuntimeException(s"byte array size (${encodedValue.size}) is not the same as Q(${Q})")
        var valueToStore = table.byteArrayXor(mask, encodedValue)

        for ((n, j) <- neighbors.zipWithIndex) {
          if (j != l) {
            valueToStore = table.byteArrayXor(valueToStore, n)
          }
        }
        table.set(L, valueToStore)
      }
    }
  }

  // Public APIs

  /**
    *
    * @param keyInput
    * @return
    */
  def getByteArray(keyInput: String) : Option[Array[Byte]] = {
    val key = if (caseSensitive) keyInput else keyInput.toUpperCase()
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    if (table.checkAllZeroElementsInTable(neighbors)) {
      None
    } else {
      for (n <- neighbors) {
        valueToRestore = table.byteArrayXor(valueToRestore, n)
      }
      Some(valueToRestore)
    }
  }

  def size = {
    non_zero_n * Q + util.conversion.Util.getBytesForBits(m)
  }

  def serialized_size = header.size + table.size

  def serialize = header.serialize ++ table.serialize
}
