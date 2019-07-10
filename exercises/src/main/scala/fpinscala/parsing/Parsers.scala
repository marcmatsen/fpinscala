package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))


  def char(c: Char): Parser[Char]

  def run[A](p: Parser[A])(input:String): Either[ParseError, A]

  def string(s: String): Parser[String]

  //def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]



  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many[B>:A]: Parser[List[A]] = self.many(p)


  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}