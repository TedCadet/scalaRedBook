import scala.Console.println
import ADT.MyList
import ADT.MyTree
// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n
  // my version
/*  def factorial(n: Int): Int =
    if n == 1 then 1
    else n * factorial(n-1)*/

// this version won't consumme call stack frames, go see 'tail position'

  /**
   *
   * @param n
   * @return : retourne le factoriel de n
   */
  def factorial(n: Int): BigInt =

    /**
     *
     * @param n : n va diminuer a chaque iteration
     * @param acc : va accummuler les multiplication de n (ex: n * 1, n-1 * n, n-2, n-2 * (n * n-1))
     * @return
     */
    @annotation.tailrec
    def go(n: Int, acc: Int): BigInt =
      if n<= 0 then acc
      else go(n -1, n * acc)

    go(n, 1)

  def fibo(n: Int) : BigInt =
    @annotation.tailrec
    def go(counter: Int, prev: Int, current: Int) : Int =
      if counter == 0 then current
      else go(counter-1, current, prev + current)

    go(n,0,1)

  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)
    loop(0)


  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  private def formatResults(name: String, x: Int, f: Int => BigInt) =
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))


  @main def printAbs: Unit =
      val ml = MyList(7,8,9,10)
      val mlTwo = MyList(11.0,12.0,13.0,14.0)
//      println(formatResults("factorial",7, factorial))
//      println(formatResults("fibonnaci",7, fibo))
//      println(findFirst(Array(7,8,9,10), (x: Int) => x == 9))
      println(MyList.mlDoubleToString(mlTwo))
//      println(MyList.append(ml,mlTwo))


