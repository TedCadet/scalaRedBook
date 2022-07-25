package chapterFourTypes

import MyOption.{MySome, None}
import MyEither.{MyLeft, MyRight}

import scala.util.control.NonFatal

object RunChapFour:

  def mean(xs: Double*): MyOption[Double] =
    if xs.isEmpty then None
    else MySome(xs.sum / xs.length)

  def meanTwo(xs: Double*): MyEither[String, Double] =
    if xs.isEmpty then MyLeft("xs is empty")
    else MyRight(xs.sum / xs.length)


  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _.map(f)

  val absO: MyOption[Double] => MyOption[Double] = lift(Math.abs)

  /**
   * Top secret formula for computing an annual car
   * insurance premium from two key factors.
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Some(None) :: tail =>
        Option(Nil)
      case head :: Nil =>
        Option(head.get :: Nil)
      case (head: Option[A]) :: tail: List[Option[A]] =>
        Option(head.get :: sequence(tail).get)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    Option(
      for
        a<- as
      yield f(a).get
    )

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  @main
  def run(): Unit =
    println("running...")

    val xs = mean(1,2,3)
    val ys = meanTwo(1,2,3)

    println( ys match
      case MyLeft(str) =>
        str
      case MyRight(db) =>
        db
    )
    println(mean())
    println(xs)
    println(xs.isEmpty)
    println(xs.getOrElse(1.0))
    println(xs.map((x: Double) => x == 2.0))

