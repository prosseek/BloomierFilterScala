package core

import org.scalatest._
/**
 * Created by smcho on 6/14/14.
 */
class ValueValidatorTest extends FunSuite {
  test("is printable") {
    assert(true == ValueValidator.isPrintable('a'))
  }
}
