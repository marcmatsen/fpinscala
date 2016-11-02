package fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling._

class OptionEitherSpec extends FunSuite with Matchers {

  test("sequence should seq") {
    val orig = List(Some(1), Some(2), Some(3))
    assert(Option.sequence(orig) == Some(List(1,2,3)))
  }

  test("sequence should return None") {
    val orig = List(Some(1), None, Some(3))
    assert(Option.sequence(orig) == None)
  }



}
