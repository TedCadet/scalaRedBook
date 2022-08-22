package chapterFive

import chapterFive.MyLazyList.{Cons, Empty, cons, empty}

enum MyLazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => MyLazyList[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => List(h()).concat(t().toList.map(a => a))

  def take(n: Int): MyLazyList[A] = this match
    case Empty => Empty
    case Cons(_, _) if n <= 0 => Empty
    case Cons(h, t) => MyLazyList.cons(h(), t().take(n-1))

  def drop(n: Int): MyLazyList[A] = this match
    case Empty => Empty
    case Cons(_, _) if n <= 0 => Empty
    case Cons(_, t)  => t().drop(n-1)

  //TODO: A etudier
  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h,t) => f(h(), t().foldRight(acc)(f))
    case _ => acc

  //TODO: A etudier
  def exists(p: A => Boolean): Boolean =
  //    this match
  //      case Cons(h, t) => p(h()) || t().exists(p)
  //      case _ => false
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): MyLazyList[A] =
//    this match
//      case Empty => Empty
//      case Cons(h, _) if !p(h()) => Empty
//      case Cons(h, t)  => MyLazyList.cons(h(), t().takeWhile(p))
    foldRight(empty[A])((a, b) => if p(a) then cons(a,b) else Empty)

  def map[B](p: A => B): MyLazyList[B] =
    foldRight(empty[B])((a, b) => cons(p(a), b))

  def flatMap[B](p: A => MyLazyList[B]): MyLazyList[B] =
    foldRight(empty[B])((a, b) => p(a).append(b))

  def filter(p: A => Boolean): MyLazyList[A] =
    foldRight(empty[A])((a, b) => if p(a) then cons(a,b) else b)

  //TODO: A etudier >:
  def append[A2 >: A](bs: => MyLazyList[A2]): MyLazyList[A2] =
    foldRight(bs)((a,b) => cons(a,b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption



object MyLazyList:
  def cons[A](h: => A, t: => MyLazyList[A]): MyLazyList[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A]: MyLazyList[A] = Empty

  def apply[A](as: A*): MyLazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): MyLazyList[A] =
    cons(a, continually(a))

  def from(n: Int): MyLazyList[Int] =
    cons(n, from(n+1))

//  def fibs(n: Int, next: Int): MyLazyList[Int] =
//    cons(n, fibs(next,n+next))

  def fibs: MyLazyList[Int] =
    def go(current: Int, next: Int): MyLazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): MyLazyList[A] =
    f(state) match
      case Some(a, s) => cons(a, unfold(s)(f))
      case None => empty

  def unfoldFibs: MyLazyList[Int] =
    unfold(0,1) {
      case (current, next) =>
        Some((current, (next, current + next)))
    }