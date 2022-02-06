package chapterFourTypes

enum MyOption[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): MyOption[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
//    case None => None
//    case Some(a) => f(a) // works
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(a) => a

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
//    case None => ob // doesn't work
    map(Some(_)).getOrElse(ob) // Some(_) is a A => B??

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then Some(a) else None) // a est fournis dans map 2e case -_-


