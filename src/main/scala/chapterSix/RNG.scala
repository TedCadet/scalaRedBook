package chapterSix

trait RNG:
//    def next[A]: (A, RNG)
    def nextInt: (Int, RNG)

