import org.scalatest.FunSuite

class MiscellaneousTests extends FunSuite {
  val misc = new Miscellaneous

  test("8 queens") {
    assert(misc.eightQueens(8).length == 92)
  }

  test("Knights") {
    val N = 8
    val allPos = for (i <- 1 to N; j <- 1 to N) yield (i, j)
    val sol1 = misc.knightTours(N, false).head
    assert(allPos.forall(sol1.contains(_)))

    // takes too long
    //val sol2 = misc.knightTours(N, true).head
    //assert(allPos.forall(sol2.contains(_)))
    //System.out.println(sol2)
  }

  test("puzzle") {
    println(misc.solvePuzzle(List(3, 5, 4, 12)))
    val result = misc.solvePuzzle(List(2, 3, 5, 7, 11))
    println(result)
    println(result.length)
  }

  test("Sudoku") {
    val problem: Array[Int] = Array(
      0, 0, 4, 8, 0, 0, 0, 1, 7,
      6, 7, 0, 9, 0, 0, 0, 0, 0,
      5, 0, 8, 0, 3, 0, 0, 0, 4,
      3, 0, 0, 7, 4, 0, 1, 0, 0,
      0, 6, 9, 0, 0, 0, 7, 8, 0,
      0, 0, 1, 0, 6, 9, 0, 0, 5,
      1, 0, 0, 0, 8, 0, 3, 0, 6,
      0, 0, 0, 0, 0, 6, 0, 9, 1,
      2, 4, 0, 0, 0, 1, 5, 0, 0
    )

    misc.sudoku(problem).print
  }

  test("Test Von Koch") {
    def exampleGraph() = {
      Map(
        "a" -> List("b", "d", "g"),
        "b" -> List("a", "c", "e"),
        "c" -> List("b"),
        "d" -> List("a"),
        "e" -> List("b", "f"),
        "f" -> List("e"),
        "g" -> List("a")
      )
    }

    val g = exampleGraph()
    val sol = misc.vonkoch(g)
    assert(sol.isDefined)
    val nodes = sol.get
    val allWeights = for (p <- g.keys; c <- g(p))
      yield Math.abs(nodes(p) - nodes(c))

    assert(allWeights.toSeq.distinct.size+1 == g.keys.size)
  }

  test("Test Nonograms") {
    val rows = List(List(3),List(2,1),List(3,2),List(2,2),List(6),List(1,5),List(6),List(1),List(2))
    val cols = List(List(1,2),List(3,1),List(1,5),List(7,1),List(5),List(3),List(4),List(3))

    // no assertions just prints the solution
    misc.printSolution(rows.size, cols.size, misc.nonogram(rows, cols).get)
  }
}
