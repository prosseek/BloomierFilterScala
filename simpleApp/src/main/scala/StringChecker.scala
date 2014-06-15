/**
 * Created by smcho on 6/8/14.
 */
class StringChecker {

  def correctCharacter(x:Char) = true

  def checkString(x:String) = {
    x.forall(correctCharacter)
  }
}

object StringChecker extends App{
  val a = new StringChecker
  println(a.checkString("Hello"))
}
