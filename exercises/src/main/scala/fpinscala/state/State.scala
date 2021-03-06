package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    if (nextInt == Int.MinValue) nonNegativeInt(nextRNG)
    else (Math.abs(nextInt), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    (nextInt.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val (nextDouble, nexterRND) = double(nextRNG)
    ((nextInt, nextDouble), nexterRND)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (c == 0) (l,r)
      else {
        val (newInt, newRNG) = r.nextInt
        go(c-1, newInt :: l, newRNG)
      }
    }
    go(count,List.empty[Int],rng)
  }

  val doubleByMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng) => {
      val (a,ar) = ra(rng)
      val (b,br) = rb(ar)
      (f(a,b), br)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((ra:Rand[A],rb:Rand[List[A]]) => map2(ra,rb)(_ :: _))
  }

  def intsBySequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    val l = List.fill(count)(int)
    sequence(l)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a,r) = f(rng)
      g(a)(r)
    }
  }

  def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

//  def map2ByFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    flatMap()
//  }

}


case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(x => {
    val (x1,r1) = run(x)
    (f(x1), r1)
  })

  //cheated!
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({
    s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    }
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State[Machine, (Int,Int)](m => {
      val machine = State[Machine,Unit](x => (Unit, x))
      val finalState = inputs.foldRight(machine)((i1,m1) => simulateStep(machine, i1))
      val resultMachine = finalState.run(m)
      ((resultMachine._2.candies, resultMachine._2.coins), resultMachine._2)
    })
  }

  def simulateStep(s: State[Machine, Unit], i: Input):State[Machine, Unit]  = {
    State((m:Machine) => {
        i match {
          case _ if   m.candies == 0 => (Unit, m)
          case Coin if m.locked => (Unit, Machine(false:Boolean, m.candies, m.coins + 1))
          case Turn if !m.locked => (Unit, Machine(true:Boolean, m.candies - 1, m.coins))
          case _ => (Unit, m)
        }
      })
    }
  }

