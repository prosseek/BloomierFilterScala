package bloomierfilter.main

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.{Map => MMap}

import bloomierfilter.core._

object ByteArrayBloomierFilter {
  val HEADER_SIZE = 2
  val UNKNOWN = -1

  private def createMapWithLowercaseKeys(keysDictInput:Map[String, Array[Byte]]) = {
    val map = collection.mutable.Map[String, Array[Byte]]()
    for ((key,value) <- keysDictInput) {
      map(key.toLowerCase()) = value
    }

    collection.immutable.Map(map.toSeq: _*)
  }

  def analyzeSerialized(byteArray:Array[Byte]) = {

    val header = Header(byteArray.slice(0, HEADER_SIZE))

    val m = header.m // header.m is shifted by two bits
    val k = header.k
    val Q = header.Q
    val hashSeed = header.hashSeed
    val q = Q * 8
    val complete = header.complete

    val serializedTable = byteArray.slice(HEADER_SIZE, byteArray.size)
    val table = new Table(m, Q)
    table.createTable(serializedTable)

    val hasher = new BloomierHasher(m = m, k = k, q = q, hashSeed = hashSeed)
    val n = UNKNOWN // use non_zero_n instead

    (m, k, q, Q, hashSeed, table, hasher, n)
  }

  def apply(byteArray: Array[Byte]):ByteArrayBloomierFilter = {
    val babf = new ByteArrayBloomierFilter
    babf.loadByteArray(byteArray)
    babf
  }

  def apply(filePath:String):ByteArrayBloomierFilter = {
    val babf = new ByteArrayBloomierFilter
    babf.load(filePath)
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
class ByteArrayBloomierFilter(val input:Map[String, Array[Byte]] = null,
                              val initialm:Int = 0, val k:Int = 3, val q:Int = 4,
                              val maxTry: Int = 5, val initialHashSeed:Int = 0, val caseSensitive: Boolean = false,
                              val force_depth_count_1:Boolean = false,
                              val force_m_multiple_by_four:Boolean = false) {

  // parameters that defines Bloomier Filter
  var Q:Int = _
  var hashSeed:Int = _
  var n:Int = _
  var m = initialm // initially m is set to the given value, it will be updated when m = 0

  // key and location map
  val keyLocationMap = MMap[String, Int]()

  if (force_m_multiple_by_four && (initialm != 0 && initialm % 4 != 0))
    throw new RuntimeException(s"You set force_m_multiply_by_four, but your initialm is not multiply by four ${initialm}")

  var non_zero_n : Int = _

  // objects that defines Bloomier filter
  var table: Table = _
  var header: Header = _
  var hasher: BloomierHasher = _

  if (input != null) {
    // 1. find the ordering of the keys
    //    1.1 we get `m` if m == 0 (user's request to calculate m)
    //    1.2 we get hashSeed that enables the ordering
    val keysDict = if (caseSensitive) input else ByteArrayBloomierFilter.createMapWithLowercaseKeys(input)
    val oamf = new OrderAndMatchFinder(keys = keysDict.keys.toList, initialM = m, k = k,
      maxTry = maxTry, initialHashSeed = initialHashSeed,
      force_depth_count_1 = force_depth_count_1,
      force_m_multiple_by_four = force_m_multiple_by_four)
    val orderAndMatch = oamf.find()
    hashSeed = orderAndMatch.hashSeed
    if (m == 0) {
      m = oamf.m
    }

    // 2. make the hasher with updated m and hashSeed
    hasher = new BloomierHasher(m = m, k = k, q = q, hashSeed = hashSeed)

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
        table(L) = valueToStore
        keyLocationMap(key) = L
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
    val key = if (caseSensitive) keyInput else keyInput.toLowerCase()
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

  def serialize = {
    // you need header for serialization
    header = Header(m = m, Q = Q, hashSeed = hashSeed, complete = if (force_depth_count_1) 1 else 0)

    if (k != 3) {
      throw new RuntimeException(s"Only k == 3 is allowed in serialization process, you provided ${k}")
    }
    if (!Set(1,2,4,8).contains(Q)) {
      throw new RuntimeException(s"Only 1/2/4/8 is allowed in serialization process, you provided ${Q}")
    }
    val serializedHeader = header.encode() // require () to use default parameters
    val serializedTable = table.serialize
    serializedHeader ++ serializedTable
  }

  /**
    *  m, m' : m = m'* 4
    *  k, q, Q, Q' : 2**Q' = Q
    *  n (total number of elements)
    *  L (locations)
    *  map (label -> index)
    * @return
    */

  def information = {
    this.serialize // to fill the header

    // m, Q, L, lToIndex is from the table
    val mmap = this.table.information

    mmap("k") = this.k
    mmap("keyToL") = keyLocationMap.toMap
    mmap("seed") = this.hashSeed
    mmap("C") = this.header.complete

    // todo:
    //   the value 1 should work now, but it should be modified with different versions
    // start location in the serialized byte array
    // 1 is for summary
    // table starts at the header.size
    mmap("startLocation") = 1 +  header.size

    mmap
  }

  def save(filePath:String, byteArray:Array[Byte] = null) = {
    var ba = byteArray
    if (byteArray == null)
      ba = serialize

    val bos = new BufferedOutputStream(new FileOutputStream(filePath))
    Stream.continually(bos.write(ba))
    bos.close()
  }

  def load(filePath:String) = {
    if (Files.exists(Paths.get(filePath))) {
      val bis = new BufferedInputStream(new FileInputStream(filePath))
      val byteArray = Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray
      loadByteArray(byteArray)
    }
    else {
      throw new RuntimeException(s"No such file exists ${filePath}")
    }
  }

  def loadByteArray(byteArray:Array[Byte]) = {
    val (m, k, q, qq, hashSeed, table, hasher, n) = ByteArrayBloomierFilter.analyzeSerialized(byteArray)

    this.Q = qq
    this.hashSeed = hashSeed
    this.n = n

    this.table = table
    this.hasher = hasher

    this.non_zero_n = table.calculate_non_zero_n
  }
}