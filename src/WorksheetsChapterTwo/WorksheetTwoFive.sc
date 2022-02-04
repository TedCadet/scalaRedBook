def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

def composeFG[A, B, C](f: B => C, g: A => B): A => C =
  a => (f compose g)(a)