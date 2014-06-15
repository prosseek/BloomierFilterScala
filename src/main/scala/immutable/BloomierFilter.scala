package immutable

import core._

/**
 * Created by smcho on 6/2/14.
 */

object BloomierFilter {
  var caseSensitive: Boolean = true
  var maxTry: Int = 5

  def checkZeroElement(element: Seq[Byte]) : Boolean = {
    element.forall(_ == 0)
  }

  //def checkAllZeroElementsInTable(neighbors: Seq[Int], table: Seq[Seq[Byte]]) : Boolean = {
  def checkAllZeroElementsInTable(neighbors: Seq[Int], table: Array[Array[Byte]]) : Boolean = {
    for (n <- neighbors) {
      if (!checkZeroElement(table(n))) return false
    }
    true
  }
}

class BloomierFilter(keysDictInput:Map[String, Any], m:Int, k:Int, q:Int, maxTry:Int = 5, initialHashSeed:Int = 0, enc:Encoder = null, dec:Decoder = null) {
  val keysDict = if (BloomierFilter.caseSensitive) keysDictInput else createMapWithUppercaseKeys(keysDictInput)
  val hasher = core.BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)
  val oamf = new core.OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, q = q, maxTry = BloomierFilter.maxTry, initialHashSeed = initialHashSeed)
  val orderAndMatch = oamf.find()
  val byteSize = hasher.getByteSize(q)

  var encoder: Encoder = enc
  var decoder: Decoder = dec

  // setup the default encoder/decoder
  if (enc == null && dec == null) {
    this.encoder = new DefaultEncoder()
    this.decoder = new DefaultDecoder()
  }

  val table = Array.ofDim[Byte](m, byteSize)
  val hashSeed = orderAndMatch.hashSeed
  create(keysDict, orderAndMatch)

  def this(context:Context, m:Int, k:Int, q:Int) =
    this(context.map, m, k, q)

  def createMapWithUppercaseKeys(keysDictInput:Map[String, Any]) = {
    val map = collection.mutable.Map[String, Any]()
    for ((key,value) <- keysDictInput) {
      map(key.toUpperCase()) = value
    }

    // returns a non-mutable map
    collection.immutable.Map(map.toSeq: _*)
  }
  def getDepth() = this.oamf.getDepthCount()

  def analyze(keysDict:Map[String, Any]) = {
    println(oamf.getOrderHistory())
    for ((key, value) <- keysDict) {
      print(key);print(">> ")
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      for (n <- neighbors) {
        print(n)
        print(":")
      }
      println()
    }
    println()
    print(orderAndMatch.toString())
  }

  def get(keyInput: String) : Option[Any] = {
    val key = if (BloomierFilter.caseSensitive) keyInput else keyInput.toUpperCase()
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    // If all of the row data in the table is zero, it means it's garbage data
    // Let's not be so smart for a while.
    // This is the routine to get the BOTTOM
    if (BloomierFilter.checkAllZeroElementsInTable(neighbors, table)) {
      //None // Bottom calculation
      Some("Bottom")
    } else {
      for (n <- neighbors) {
        valueToRestore = utils.conversion.Utils.byteArrayXor(valueToRestore, table(n))
      }
      decoder.decode(key, valueToRestore, byteSize)
    }
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
        val encodedValue = encoder.encode(key, keysDict(key), byteSize)
        var valueToStore = utils.conversion.Utils.byteArrayXor(mask, encodedValue)

        for ((n, j) <- neighbors.zipWithIndex) {
          if (j != l) {
            valueToStore = utils.conversion.Utils.byteArrayXor(valueToStore, table(n))
          }
        }
        table(L) = valueToStore
      }
    }
  }

  /**
   * Get the size of r (m that is not zero)
   * @return
   */
  def getSize() = {
    m - this.table.count(p => p.forall(_ == 0))
  }

  def size() = {
    getSize() * byteSize
  }

  def printContents() : Unit = printContents(keysDict.keys)

  def printContents(keys: Iterable[String]) : Unit = {
    println("BL---------------------------------------------")
    println(s"Size in bytes : ${size()}")
    for (key <- keys) {
      println(s"KEY($key) => ${get(key).getOrElse(null)}")
    }
    println("---------------------------------------------BL")
  }
}
