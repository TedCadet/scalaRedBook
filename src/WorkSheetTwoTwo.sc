def isSorted[A](as:Array[A], gt: (A,A) => Boolean): Boolean =
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if n == as.length-1 then true
    else if gt(as(n),as(n+1)) then false
    else loop(n+1)

  loop(0)

val ad: Array[Int] = Array[Int](1,2,3,4)
val ae: Array[Int] = Array[Int](1,2,4,3)
val as: Array[Int] = Array[Int](1,2,3)


def gt(x: Int,y: Int): Boolean =
  if x > y then true
  else false

println(isSorted(ad,gt))
println(isSorted(ae,gt))
println(isSorted(as,gt))
