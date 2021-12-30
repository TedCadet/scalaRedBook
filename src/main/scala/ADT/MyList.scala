package ADT

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: MyList[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def tail[A](as: MyList[A]): MyList[A] = as match
    case Nil => sys.error("tail of empty list")
    case Cons(_, Nil) => sys.error("tail of empty list")
    case Cons(_, t) => t

  def head[A](a: A, as: MyList[A]): MyList[A] = as match
    case Nil => sys.error("empty list")
    case Cons(_, tl) => Cons(a, tl)

  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def drop[A](as: MyList[A], n: Int): MyList[A] =
    if n <= 0 then as
    else as match
      case Cons(_,tl) => drop(tl, n-1)
      case Nil => Nil

  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = as match
    case Cons(hd,tl) if f(hd) => dropWhile(tl, f)
    case _ => as

  def init[A](as: MyList[A]): MyList[A] = as match
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))

  def foldRight[A, B] (as: MyList[A], acc: B, f: (A, B) => B) : B = as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs,acc,f))

  @annotation.tailrec
  def foldLeft[A, B] (as:MyList[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(hd, tl) => foldLeft(tl,f(acc, hd),f)


  def sumWithFoldRight(ns: MyList[Int]) =
    // call foldRight avec une fx anonyme pour faire une somme
    foldRight(ns, 0, (x,y) => x + y)

  def productWithFoldRight(ns: MyList[Int]) =
    foldRight(ns, 1.0, _ * _)

  def sumWithFoldLeft(ns: MyList[Int]) =
  // call foldRight avec une fx anonyme pour faire une somme
    foldLeft(ns, 0, (x,y) => x + y)

  def productWithFoldLeft(ns: MyList[Int]) =
    foldLeft(ns, 1.0, _ * _)

  def length[A](ns: MyList[A]): Int =
    foldRight(ns, 0, (_,acc) => acc + 1)

  def reverseWithFold[A](as: MyList[A]): MyList[A] =
    foldLeft(as, Nil: MyList[A], (acc,a) => Cons(a, acc))

  def append[A](xs: MyList[A], ys: MyList[A]) : MyList[A] =
    foldRight(xs, ys, Cons(_, _))

  def concat[A](ml: MyList[MyList[A]]): MyList[A] =
    foldRight(ml, Nil: MyList[A], append)

  def addOne(ml: MyList[Int]) : MyList[Int] =
    foldRight(ml, Nil: MyList[Int], (i, acc) => Cons(i + 1, acc))

  def mlDoubleToString [A](ml: MyList[Double]) : MyList[String] =
    foldLeft(ml, Nil: MyList[String], (acc, d) => Cons(d.toString(), acc))

//  def map[A, B](as: MyList[A], f: A => B) : MyList[B] =
//    foldLeft(as, Nil: MyList[B], (tl, i) => Cons(f(i), tl))

  def map[A,B](as: MyList[A], f: A => B): MyList[B] =
    foldRight(as, Nil: MyList[B], (a, acc) => Cons(f(a), acc))

//  def filter[A](as: MyList[A], f: A => Boolean) : MyList[A] =
//    foldLeft(as, Nil: MyList[A], (tl, i) => if f(i) then Cons(i, tl) else tl)

  def filter[A](as: MyList[A], f: A => Boolean): MyList[A] =
    foldRight(as, Nil: MyList[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

