package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(Nil:List[A])(_ :: _)

//  def take(n: Int): Stream[A] = {
//    def go(nn: Int, to: Stream[A], from: Stream[A]): Stream[A] = {
//      from match {
//        case Empty => to
//        case Cons(xh, xt) => if (nn == 0) to else go(nn - 1, Cons(xh, () => to), xt())
//      }
//    }
//    go(n, Empty[A], this)
//  }

//  def take(n: Int): Stream[A] = {
//     this match {
//      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
//   //   case Cons(h,t) => cons(h(), t().take(n-1))
//      case Cons(h,_) => this
//      case _ = empty
//  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => t().drop(n - 1)
    case Cons(h,t) if n == 1 => t()
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

//  def takeWhileByFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (!p(a)) () => b else cons(a, b) )

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else empty )

  def headOption: Option[A] = foldRight(None:Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a),b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)

  // why did the answer need this covariant nonsense
  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t) )

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h).append(t))

//  def mapByUnfold[B](f: A => B) = Stream.unfold(this) {
//        case Some(h,t) => Some((f(h()), t()))
//        case None => None
//      }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h,t), n) if n > 0 => Some((h(),(t(),n-1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean) = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(),t())
    case _ => None
  }

  def zipWithViaUnfold[B,C](f: (A,B) => C)(s2: Stream[B]): Stream[C] = unfold(this, s2) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s2) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(), t2()))
    case (Cons(h1,t1),empty) => Some((Some(h1()),None), (t1(), empty))
    case (empty,Cons(h2,t2)) => Some((None,Some(h2())), (empty, t2()))
    case _ => None
  }


//  def takeViaUnfold(n: Int): Stream[A] =
//    unfold((this,n)) {
//      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
//      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
//      case _ => None
//    }



  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, newState)) => cons(value, unfold(newState)(f))
    case None => empty
  }

  def fromByUnfold(n: Int) = unfold(n)(x => Some(x, x+1))

  def constantFromUnfold[A](a: A) = unfold(a)(x => Some(x,x))

//  def mapByUnfold[A,B](f: A => B)(s: Stream[A]) = unfold(s) {
//    case Some(h,t) => Some((f(h()), t()))
//    case None => None
//  }


}