import chapterSix.{RNG, SimpleRNG}

val rng = SimpleRNG(42)

val (i,r) = SimpleRNG.int(rng)

val (n1, rng2) = rng.nextInt

val (n2, rng3) = rng2.nextInt

println(SimpleRNG.nonNegativeInt(rng2))
println(SimpleRNG.double(rng))
println(SimpleRNG.ints(10)(rng))
SimpleRNG.unit(1)
SimpleRNG.nonNegativeEven(rng)