package util

object Check {
  /**
   * Check if input list has no duplication
   *
   * @param list
   * @tparam T
   */
  def noDuplication[T](list:Seq[T]) = {
    val size = list.size
    if (size == 0) true
    else {
      val set = list.toSet
      set.size == size
    }
  }
}