package fpinscala.datastructures

import org.scalatest._
import fpinscala.datastructures._

class TreeSpec extends FunSuite with Matchers {

  val testTree = Branch(Branch(Leaf(1),Leaf(2)), Leaf(3))

  test("size should count nodes correctly") {
    assert(Tree.size(testTree) == 5)
  }

  test("maximum gets maximum") {
    assert(Tree.maximum(testTree) == 3)
  }

  test("depth depths") {
    assert(Tree.depth(testTree) == 3)
  }

  test("map maps") {
    val doubledTree = Branch(Branch(Leaf(2),Leaf(4)), Leaf(6))
    assert(Tree.map(testTree)(_ * 2) == doubledTree)
  }

  test("size via fold") {
    assert(Tree.sizeViaFold(testTree) == 5)
  }

  test("maximum via fold") {
    assert(Tree.maximumViaFold(testTree) == 3)
  }

  test("depth via fold") {
    assert(Tree.depthViaFold(testTree) == 3)
  }

  test("mapViaFold") {
    val doubledTree = Branch(Branch(Leaf(2),Leaf(4)), Leaf(6))
    assert(Tree.mapViaFold(testTree)(_ * 2) == doubledTree)
  }


}
