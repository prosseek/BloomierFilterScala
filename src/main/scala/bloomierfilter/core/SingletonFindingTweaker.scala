package bloomierfilter.core

object SingletonFindingTweaker {
  val NONSINGLETON = -1
}

class SingletonFindingTweaker (val keys:Seq[String], hasher: BloomierHasher) {
  val nonSingletons = getNonSingletons()

  def getNonSingletons() = {
    val nonSingletons = scala.collection.mutable.Set[Int]()
    val hashesSeen = scala.collection.mutable.Set[Int]()

    for (k <- keys) {
      for (n <- hasher.getNeighborhood(k)) {
        if (hashesSeen.contains(n)) nonSingletons += n
        hashesSeen += n
      }
    }

    nonSingletons
  }

  /**
   * returns index (i) when duplication found
   * or return -1 to indicate it's not singleton
   *
   * @param key
   * @return
   */
  def tweak(key: String) :Int = {
    var i = 0

    for (n <- hasher.getNeighborhood(key)) {
      // non singleton contains this -> singleton
      if (this.nonSingletons.contains(n) == false) {
        return i
      }
      i += 1
    }
    return SingletonFindingTweaker.NONSINGLETON
  }
}
