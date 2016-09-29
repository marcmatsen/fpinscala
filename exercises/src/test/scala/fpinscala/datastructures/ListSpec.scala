package fpinscala.datastructures

import org.scalatest._
import fpinscala.datastructures._

class ListSpec extends FunSuite with Matchers {


  test("reverse should reverse a list") {
    def l1 = List(1, 2, 3)
    assert(List.reverse(l1) == List(3, 2, 1))
  }

  test("filter should filter") {
    val even = (x:Int) => x % 2 == 0
    assert(List.filter(List(1,2,3),even) == List(2))
  }




}

