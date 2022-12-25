package chapterSeven


import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future, TimeUnit}

//opaque type Future[+A] = (A => Unit) => Unit
opaque type Par[A] = ExecutorService => Future[A]

object Par:

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(eventIfRunning: Boolean): Boolean = false

  /**
   * unit promotes a constant value to a parallel computation.
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: => A): Par[A] = es => UnitFuture(a)
//  def unit[A](a: A): Par[A] = es => cb => cb(a)
  /**
   * fork marks a computation for concurrent evaluation.
   * The evaluation won’t actually occur until forced by run.
   * @param a
   * @tparam A
   * @return
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {def call = a(es).get })
//    es => cb => eval(es)(a(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
//  def get[A](pa: Par[A]): A = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A=> B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    parList.map(_.sorted)

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(pas.toIndexedSeq).map(_.toList)
  //  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  //    es => UnitFuture(ps.map(p => p(es).get()))
  //      UnitFuture(
  //        for
  //          p <- ps
  //        yield p(es).get()
  //      )

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val filteredAs: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(filteredAs).map(_.flatten )
    }

  // TODO: find a way to generalise it
  def aggregateInt(as: List[Int])(f: (Int, Int) => Int): Par[Int] = unit(as.foldRight(0)(f))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get


  extension [A](pa: Par[A])
    /**
     * run extracts a value from a Par by actually performing the computation.
     */
    def run(s: ExecutorService): Future[A] = pa(s)
//    def run(es: ExecutorService): A =
//      val ref = new AtomicReference[A]
//      val latch = new CountDownLatch(1)
//      pa(es) { a => ref.set(a); latch.countDown }
//      latch.await
//      ref.get

    /**
     * map2 combines the results of two parallel computations with a binary function.
     * @param pb
     * @param f
     * @tparam B
     * @tparam C
     * @return
     */
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        val futureA: Future[A] = pa(es)
        val futureB: Future[B] = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C]:
        private val futureA = pa(es)
        private val futureB = pb(es)
        @volatile private var cache: Option[C] = None

        def isDone = cache.isDefined

        def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)
        def get(timeout: Long, units: TimeUnit) =
          val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, units)
          val started = System.nanoTime
          val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
          val elapsed = System.nanoTime - started
          val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
          val c = f(a, b)
          cache = Some(c)
          c

        def isCancelled = futureA.isCancelled || futureB.isCancelled

        def cancel(evenIfRunning: Boolean) =
          futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)