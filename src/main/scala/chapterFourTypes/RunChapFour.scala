package chapterFourTypes

import MyOption.{Some, None}

object RunChapFour:

  def mean(xs: Double*): MyOption[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  @main
  def run(): Unit =
    println("running...")

    val xs = mean(1,2,3)
    println(mean())
    println(xs)
    println(xs.getOrElse(1.0))
    println(xs.map((x: Double) => x == 2.0))

