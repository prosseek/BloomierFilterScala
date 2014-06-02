package utils.collection

object Utils {
  /**
   * Check if input list has no duplication
   *
   * @param list
   * @tparam T
   */
  def noDuplication[T](list:List[T]) = {
    val size = list.length
    if (size == 0) true
    else {
      val set = list.toSet
      set.size == size
    }
  }
}