def failingFn(i: Int): Int =
//  val y: Int = throw new Exception("fail!")
  try
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int)
//    x + y
  catch
    case e: Exception => 43

failingFn(2)

def mean(xs: Double*): Double =
  if xs.isEmpty then
    throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length

def mean(xs: IndexedSeq[Double], onEmpty: Double): Double =
  if xs.isEmpty then onEmpty
  else xs.sum / xs.length

mean(IndexedSeq(), 2)
