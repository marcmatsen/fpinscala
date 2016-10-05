package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(z) => z
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(z) => Leaf[B](f(z))
    case Branch(l,r) => Branch[B](map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)((x) => 1)((y,z) => y + z + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)((x) => x)((y,z) => y max z)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)((x) => 1)((y,z) => (y max z) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A=>B): Tree[B] = fold(t)((x) => Leaf(f(x)): Tree[B])((y: Tree[B],z: Tree[B]) => Branch(y,z))

}