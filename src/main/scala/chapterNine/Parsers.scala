package chapterNine

import scala.annotation.targetName
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]:
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]
  def jsonParser[Err, Parser[+_]](p: Parsers[Err, Parser]): Parser[JSON]
  def document: Parser[JSON] = array | obj
  def array: Parser[JSON] = ???
  def obj: Parser[JSON] = ???
//  def whitespace: Parser[String] = Parser.regex("\\s*".r)
  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
//    infix def or(p2: Parser[A]): Parser[A]
//    def orString(s1: String, s2: String): Parser[String]
    @targetName("or")
    def |(p2: => Parser[A]): Parser[A]
    @targetName("product")
    def **[B](p2: => Parser[B]): Parser[(A, B)] = ???
//      for
//        a <- p
//        b <- p2
//      yield (a, b)
    def listOfN(n: Int): Parser[List[A]] = ???
//      if n <= 0 then
//        succeed(Nil)
//      else
//        p.map2(p.listOfN(n-1))(_ :: _)
    def many: Parser[List[A]] = ???
//      p.map2(p.many)(_ :: _) | succeed(Nil)
    def many1: Parser[List[A]] = p.map2(p.many)(_ :: _)

//    def howMany: Parser[Int]
//    def howManyAtLeastOne: Parser[Int]
    def howManyPairOfPattern(inputTwo: Char):Parser[(Int, Int)]
    def map[B](f: A => B): Parser[B] =
      for
        a <- p
      yield f(a)
    def flatmap[B](f: A => Parser[B]): Parser[B]
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      (p ** p2).map((a, b) => f(a,b))
//      for
//        a <- p
//        b <- p2
//      yield f(a,b)
    def succeed(a: A): Parser[A] =
      string("").map(_ => a)
    def slice: Parser[String]
    def defer(p2: => Parser[A]): Parser[A]
    def regex(r: Regex): Parser[String]