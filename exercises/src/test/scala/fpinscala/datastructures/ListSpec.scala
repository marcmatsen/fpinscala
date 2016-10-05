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

  test("append should append") {
    val l1 = List(1,2)
    val l2 = List(3,4)
    assert(List.appendUsingFold(l1, l2) == List(1,2,3,4))
  }

  test("concatenateLists should concatenate") {
    val l = List(List(1,2), List(3,4), List(5,6))
    assert(List.concatenateLists(l) == List(1,2,3,4,5,6))
  }

  test("flatmap flatmaps") {
    val f = (x: Int) => List(x, x*2)
    val ol = List(1,2,3)
    assert(List.flatMap(ol)(f) == List(1,2,2,4,3,6))
  }

  test("filterViaFlatmap filters") {
    val even = (x:Int) => x % 2 == 0
    assert(List.filterViaFlatMap(List(1,2,3),even) == List(2))
  }

  test("zipswith") {
    val f = (x:Int,y:Int) => x + y
    val l1 = List(1,2)
    val l2 = List(4,3)
    assert(List.zipWith(l1,l2)(f) == List(5,5))
  }

}

