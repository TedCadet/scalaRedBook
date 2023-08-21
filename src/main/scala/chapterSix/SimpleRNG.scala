package chapterSix

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG:

//  val int: Rand[Int] = _.nextInt

  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object SimpleRNG:
  val int: Rand[Int] = _.nextInt
  val double: Rand[Double] = map(nonNegativeInt)(d => d/(Int.MaxValue.toDouble + 1))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double,int)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  //    rng =>
//      val(a, rng2) = s(rng)
//      (f(a), rng2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  //    rng =>
//      val(a, rng2) = ra(rng)
//      val(b, rng3) = rb(rng2)
//      (f(a, b), rng3)

  def flatMap[A, B](r: Rand[A])(f: A=> Rand[B]): Rand[B] =
    rng =>
      val (a, _) = r(rng)
      f(a)(rng)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_,_))

  /**
   * We use a foldRight over the supplied list of RNG actions, starting with a unit(Nil: List[A])
   * —an RNG action that passes through the RNG parameter without using it, returning an empty list.
   * For each action in the original list, we compute an output of type Rand[List[A]]
   * using an input element of type Rand[A] and an accumulated Rand[List[A]].
   * We use map2 to combine those actions in to a new action, consing the output value of the first action
   * on to the output list of the accumulated action.
   * @param rs
   * @tparam A
   * @return
   */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def nonNegativeInt: Rand[Int] =
    rng =>
      rng.nextInt match
        case (n, nextState) if n >= 0 && n < Int.MaxValue  =>
          (n, nextState)
        case (_, nextState) =>
          nonNegativeInt(nextState)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if i + (n-1) - mod >= 0 then
        unit(mod)
      else nonNegativeLessThan(n)
    }

//  def double: Rand[Double] =
//    map(nonNegativeInt)(d => d/(Int.MaxValue.toDouble + 1))
//    val (i, r) = SimpleRNG.nonNegativeInt(rng)
//    (i/(Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r) = rng.nextInt
    val (d, r2) = SimpleRNG.double(r)
    ((i, d),r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r) = SimpleRNG.double(rng)
    val (i, r2) = r.nextInt
    ((d,i), r2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d, r) = SimpleRNG.double(rng)
    val (d1, r1) = SimpleRNG.double(r)
    val (d2, r2) = SimpleRNG.double(r1)
    ((d,d1,d2), r2)

  def ints(counts: Int)(rng: RNG): (List[Int], RNG) =
    counts match
      case counts if counts <= 0 =>
        (Nil, rng)
      case _ =>
        val (i, r) = rng.nextInt
        (i :: ints(counts - 1)(r)._1, r)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)







