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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val(a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match
      case (n, nextState) if n >= 0 && n < Int.MaxValue  =>
        (n, nextState)
      case (_, nextState) =>
        nonNegativeInt(nextState)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(d => d/(Int.MaxValue.toDouble + 1))
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







