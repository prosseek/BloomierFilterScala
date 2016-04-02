package bloomierfilter.core

/**
  * Created by smcho on 4/1/16.
  */
class Table(val m:Int, val Q:Int) {
  val table = Array.ofDim[Byte](m, Q)
}
