package core

/**
 * Created by smcho on 6/1/14.
 */
class SingletonFindingTweaker (val keysDict:Map[String, Any], hasher: BloomierHasher) {
  val nonSingletons = getNonSingletons()

  def getNonSingletons() = {
    val nonSingletons = scala.collection.mutable.Set[Int]()
    val hashesSeen = scala.collection.mutable.Set[Int]()

    for (k <- keysDict.keys) {
      for (n <- hasher.getNeighborhood(k)) {
        if (hashesSeen.contains(n)) nonSingletons += n
        hashesSeen += n
      }
    }

    nonSingletons
  }

  def tweak(key: String) :Int = {
    var i = 0

    for (n <- hasher.getNeighborhood(key)) {
      if (!this.nonSingletons.contains(n)) {
        return i
      }
      i += 1
    }
    return -1
  }
}
