def factorial(n: Int): Int =
  if n == 1 then 1
  else n * factorial(n-1)

def fiboOri(n: Int): Int =
  if n == 0 then 1
  else if n == 1 then 1
  else n + fiboOri(n+1)

def fibo(n: Int) : Int =
  @annotation.tailrec
  def go(counter: Int, prev: Int, current: Int) : Int =
    if counter == 0 then current
    else go(counter-1, current, prev + current)

  go(n,0,1)

@annotation.tailrec
def fiboTwo(counter: Int, prev: Int, current: Int) : Int =

  if counter == 0 then current
  else fiboTwo(counter-1, current, prev + current)

println(fiboTwo(6,0,1))