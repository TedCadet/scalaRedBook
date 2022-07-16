package chapterFourTypes

enum MyOption[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): MyOption[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def map2[A, B, C](a:MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    (a, b) match
      case (Some(a: A), Some(b: B)) =>
        Some(f(a,b))
      case (Some(None), Some(_)) | (Some(_), Some(None)) =>
        None

  def isEmpty: Boolean = this match
    case Some(_) => false
    case None => true

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
//    case None => None
//    case Some(a) => f(a) // works
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(a) => a

//  def get[A](): A = this match
//    case Some(a) => a
//    case None => None

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
//    case None => ob // doesn't work
    map(Some(_)).getOrElse(ob) // Some(_) is a A => B??

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then Some(a) else None) // a est fournis dans map 2e case -_-


