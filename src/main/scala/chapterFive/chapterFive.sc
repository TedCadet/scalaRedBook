import chapterFive.MyLazyList

def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if cond then onTrue() else onFalse()

if2(23 < 22,
  () => println("a"),
  () => println("b")
)

def maybeTwice(b: Boolean, i: => Int) =
  if b then i+i else 0

def maybeTwice2(b: Boolean, i: => Int) =
  lazy val j = i
  if b then j+j else 0

// hi sera print deux fois, a chacune des evaluations de i
val x = maybeTwice(true, { println("hi"); 1+41})

// "lazy" va cache le resultat et evaluer le val qu'une seul fois
// aussi, la valeur ne sera qu'evaluer que lors de la premiere fois qu'elle sera evaluer
// le resultat conserver sera utiliser pour les deux appels de "j"
val y = maybeTwice2(true, { println("hi"); 1+41})
LazyList(1,2,3).takeWhile(n => n < 3).toList


MyLazyList.continually(2).take(5).toList
MyLazyList.from(1).take(5).toList
MyLazyList.fibs(0,1).take(8).toList
