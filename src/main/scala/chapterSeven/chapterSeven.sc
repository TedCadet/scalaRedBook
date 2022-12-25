import chapterSeven.Par


def a: Int = 5
//Par.
//def countWordsListParagraphs(listParagraphs: List[String]): Par[Int] =
//  val listOfCounts = Par.parMap(listParagraphs)(countWordsInParagraph)
//
//  listOfCounts.map(x => Par.aggregateInt(x)(_ + _))



def countWordsInParagraph(paragraph: String): Int = paragraph.split(" ").length

//def test: (Int, Int) => Int = _ + _
//val as = List(1,2,3)
//as.foldRight(0)(test)
