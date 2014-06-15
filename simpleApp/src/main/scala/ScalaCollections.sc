import scala.collection.mutable.BitSet

def hello(x: collection.immutable.BitSet) =
  println(x.mkString("*"))
def hello(x: collection.mutable.BitSet) =
  println(x.mkString("*"))

val a = BitSet()
a += 10
a ++= BitSet(10,20,30)

hello(a)
