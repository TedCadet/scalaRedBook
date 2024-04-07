package chapterten

object RunChapTen:

  val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 + a2
    val empty = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    val empty = 0

  val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val empty = false

  val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val empty = false
  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val empty = None

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
      val empty = identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l,r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))
  @main
  def run(): Unit =
    println("--- chap ten ---")

