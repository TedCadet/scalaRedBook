package chapterNine

trait Parsers[ParseError, Parser[+_]]:
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
    infix def or(p2: Parser[A]): Parser[A]
    def |(p2: Parser[A]): Parser[A] = p.or(p2)
    def listOfN(n: Int): Parser[List[A]]
    def howMany(input: String): Either[ParseError, A]
    def howManyAtLeastOne(input: String): Either[ParseError, A]

    













