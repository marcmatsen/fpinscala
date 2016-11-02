package fpinscala.laziness

import org.scalatest._
import fpinscala.laziness._

class StreamSpec extends FunSuite with Matchers {

  test("toList should toList") {
    assert(Stream(1,2,3).toList == List(1,2,3))
  }

  test("forall if not all are for") {
    assert(!Stream(1,2,3).forAll(_ % 2 == 0))
  }

  test("forall if all are for") {
    assert(Stream(1,2,3).forAll(_ < 5))
  }

  test("flatmap") {
    assert(Stream(1,2).flatMap((x) => Stream(x,x)).toList == Stream(1,1,2,2).toList)
  }

  test("from") {
    assert(Stream.from(3).take(3).toList == List(3,4,5))
  }

  test("take via unfold") {
    assert(Stream(1,2,3,4).takeViaUnfold(2).toList == List(1,2))
  }

  test("zipwithviaunfold") {
    val s1 = Stream(1,2,3)
    val s2 = Stream(5,5,5,5)
    val zipped = s1.zipWithViaUnfold((x:Int,y:Int) => x + y)(s2)
    assert(zipped.toList == List(6,7,8) )
  }

}
