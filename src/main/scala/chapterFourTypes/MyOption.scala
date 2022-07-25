package chapterFourTypes

enum MyOption[+A]:
  case MySome(get: A)
  case None

  def map[B](f: A => B): MyOption[B] = this match
    case None => None
    case MySome(a) => MySome(f(a))

  def map2[A, B, C](a:MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    (a, b) match
      case (MySome(a: A), MySome(b: B)) =>
        MySome(f(a,b))
      case (MySome(None), MySome(_)) | (MySome(_), MySome(None)) =>
        None

  def isEmpty: Boolean = this match
    case MySome(_) => false
    case None => true

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
//    case None => None
//    case MySome(a) => f(a) // works
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case MySome(a) => a

//  def get[A](): A = this match
//    case MySome(a) => a
//    case None => None

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
//    case None => ob // doesn't work
    map(MySome(_)).getOrElse(ob) // MySome(_) is a A => B??

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then MySome(a) else None) // a est fournis dans map 2e case -_-


