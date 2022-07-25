package chapterFourTypes

import scala.util.control.NonFatal

enum MyEither[+E, +A]:
  case MyLeft(value: E)
  case MyRight(value: A)

  def map[B](f: A => B): MyEither[E, B] = this match
    case MyRight(a: A) => MyRight(f(a))
    case MyLeft(e: E) => MyLeft(e)

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = ???

  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = ???
