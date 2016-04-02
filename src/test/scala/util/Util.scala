package util

/**
  * Created by smcho on 4/1/16.
  */
object Util {
  def noDuplication[T](list:List[T]) = {
    val size = list.length
    if (size == 0) true
    else {
      val set = list.toSet
      set.size == size
    }
  }
}
