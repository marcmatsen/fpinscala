package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest._

class StateSpec extends FunSuite with Matchers {

  test("ints") {
    val l = RNG.ints(5)(Simple(1))
    println(l._1)
  }

  test("intsbysequence") {
    //val s:RNG = Simple(1)
    val l = RNG.intsBySequence(5)(Simple(1))
    println(l._1)
  }

}