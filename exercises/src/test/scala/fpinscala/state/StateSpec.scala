package fpinscala.state

import fpinscala.state
import fpinscala.state.State
import fpinscala.state.RNG.Simple
import fpinscala.state._
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

  test("coin transition") {
    val inputs = List(Coin)
    val simulator = State.simulateMachine(inputs)
    val machine = Machine(true:Boolean, 2, 0)
    val ((candies, coins), resultMachine) = simulator.run(machine)
    assert(candies == 2)
    assert(coins == 1)
    assert(!resultMachine.locked)
  }

  test("turn transition") {
    val inputs = List(Turn)
    val simulator = State.simulateMachine(inputs)
    val machine = Machine(false:Boolean, 2, 1)
    val ((candies, coins), resultMachine) = simulator.run(machine)
    assert(candies == 1)
    assert(coins == 1)
    assert(resultMachine.locked)
  }

}