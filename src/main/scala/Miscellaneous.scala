class Miscellaneous {
  // P90
  def eightQueens(N: Int): List[List[Int]] = {
    val cols = (1 to N).toList

    def checkPos(pos: List[Int]): Boolean =
      pos.zip(cols).combinations(2).forall({
        case (r1,c1) :: (r2,c2) :: _ => Math.abs(r1-r2) != Math.abs(c1-c2)
      })

    cols.permutations.filter(checkPos).toList
  }

  // P91 - with laziness and closed solutions
  type PosList = List[(Int, Int)]
  val knightOffsets = List((-1,-2),(-1,2),(-2,-1),(-2,1),(1,-2),(1,2),(2,-1),(2,1))

  def knightTours(N: Int, onlyClosed: Boolean): Stream[PosList] = {
    def jumps(pos: (Int, Int)): PosList =
      knightOffsets.map(p => (p._1+pos._1, p._2+pos._2))
        .filter({case (x,y) => x>0 && y>0 && x<=N && y<=N})

    def find(current: (Int, Int), visited: PosList): Stream[PosList] =
      if (visited.length == N*N)
        Stream(visited.reverse)
      else
        jumps(current).filter(!visited.contains(_)).toStream
            .foldLeft(Stream[PosList]())((s, p) => s #::: find(p, p::visited))

    val allPos = for (i <- 1 to N; j <- 1 to N) yield (i, j)
    val result = allPos.toStream.flatMap(p => find(p, List(p)))

    if (onlyClosed)
      result.filter(moves => jumps(moves.last).contains(moves.head))
    else
      result
  }

  // P92
  type Graph = Map[String, List[String]]

  def vonkoch(g: Graph): Option[Map[String, Int]] = {
    g.keys.toSeq.permutations.map(_.zipWithIndex.toMap.mapValues(_+1))
      .find(map => {
        val edges = for (p <- g.keys; c <- g(p))
          yield Math.abs(map(p) - map(c))

        edges.toSeq.distinct.size == map.size - 1
      })
  }

  // P93
  type Op = Node[Either[Char, Int]]

  val functions = Map[Char, (Int, Int) => Int](
    '+' -> (_ + _), '-' -> (_ - _),
    '*' -> (_ * _), '/' -> (_ / _)
  )
  val priorities = Map[Char, Int](
    '+' -> 2, '-' -> 2, '*' -> 1, '/' -> 1
  )
  val assoc = List('+', '*')
  val operators = functions.keySet

  def str(tree: Op): String = tree match {
    case Node(Left(op), left: Op, right: Op) =>
      def pp(tree: Op): String = {
        val s = str(tree)
        tree.value match {
          case Left(o) =>
            // operator precedence is crude
            val (p, pUp) = (priorities(o), priorities(op))
            if (p < pUp || (p == pUp && assoc.contains(o))) s
            else "("+s+")"
          case _ => s
        }
      }
      pp(left) + op + pp(right)
    case Node(Right(n), _, _) => n.toString
  }

  def eval(tree: Op): Int = tree match {
    case Node(Left(op), left: Op, right: Op) => functions(op)(eval(left), eval(right))
    case Node(Right(n), _, _) => n
  }

  def solvePuzzle(puzzle: List[Int]): List[String] = {
    def rollingSplit(list: List[Int]) =
      for (i <- 1 until list.length)
        yield list.splitAt(i)

    def allTreesFor(input: List[Int]): List[Op] =
      if (input.length == 1)
        List(Node(Right(input.head)))
      else
        rollingSplit(input).flatMap({case (l, r) =>
          operators.flatMap(op =>
            for (lt <- allTreesFor(l); rt <- allTreesFor(r))
              yield Node(Left(op), lt, rt)
          )
        }).toList

    rollingSplit(puzzle).flatMap({case (l, r) =>
      val all = for (lt <- allTreesFor(l); rt <- allTreesFor(r)) yield (lt, rt)
      all.filter({case (lt, rt) =>
        try {
          eval(lt) == eval(rt)
        } catch {
          // division by 0 is not valid
          case _: ArithmeticException => false
        }
      })
    }).map({case (lt, rt) => str(lt) + '=' + str(rt)}).distinct.toList
  }

  // P95
  class Board(val board: Array[Set[Int]]) {
    def this(problem: Seq[Int]) {
      this(problem.map({
        case 0 => (1 to 9).toSet
        case n => Set(n)
      }).toArray)
    }

    def complete = board.forall(_.size == 1)

    def get(i: Int, j: Int) = board(i * 9 + j)

    def set(i: Int, j: Int, set: Set[Int]) = board(i * 9 + j) = set

    def print = board.view.zipWithIndex.foreach {
      case (set, i) =>
        System.out.print(" " + (if (set.size == 1) set.head else ".") + " ")
        if ((i+1)%9 == 0) println()
    }
  }

  def nums(set: Seq[Set[Int]]) = set.filter(_.size == 1).map(_.head).toSet

  def squareOf(i: Int, j: Int) = {
    val row = i/3*3
    val col = j/3*3
    for (x <- row until row+3; y <- col until col+3)
      yield (x, y)
  }

  def sudoku(problem: Seq[Int]): Board = {
    val board = new Board(problem)
    val range = 0 until 9
    while (!board.complete) {
      for (i <- range; j <- range) {
        val current = board.get(i, j)
        if (current.size > 1)
          board.set(i, j,
            current
              .diff(nums(range.filter(_ != i).map(board.get(_, j)))) // row
              .diff(nums(range.filter(_ != j).map(board.get(i, _)))) // col
              .diff(nums(squareOf(i, j).map({case (x,y)=>board.get(x,y)}))) // square
          )
      }
    }

    board
  }

  // P98 - Brute force with some laziness
  type Solution = List[(Int, Int)]
  type FillList = List[List[Int]]

  def getcol(t: Solution, i: Int): List[Int] = t.filter(_._2==i).map(_._1)
  def getrow(t: Solution, i: Int): List[Int] = t.filter(_._1==i).map(_._2)

  def filling(v: List[Int]) = v match {
    case List() => List()
    case h :: tail =>
      tail.foldLeft((h, List(1)))({
        case ((last, count::t), n) =>
          if ((n-last) == 1) (n, (count+1)::t)
          else (n, 1::count::t)
      })._2.reverse
  }

  def isSolution(rows: FillList, cols: FillList, sol: Solution) = {
    def check(v: FillList, f: (Solution, Int) => List[Int]) =
      v.zipWithIndex.forall({case (fill, i) => filling(f(sol, i)) == fill})

    check(rows, getrow) && check(cols, getcol)
  }

  def nonogram(rows: FillList, cols: FillList) = {
    val Y = cols.size

    def solved(sol: Solution) = isSolution(rows, cols, sol)

    def solve(rows: FillList, sol: Solution): Option[Solution] = {
      rows.zipWithIndex.filter(_._1.nonEmpty) match {
        case List() => if (solved(sol)) Some(sol) else None

        case (len::t, rown) :: _ =>
          val rowFills = getrow(sol, rown)
          val init = if (rowFills.isEmpty) 0 else rowFills.last+2
          if (init >= Y || (init + len) >= Y)
            None
          else {
            val solutions = (init to (Y - len)).view.map( j =>
              solve(rows.updated(rown, t), sol ++ (j until j+len).map((rown, _))))

            solutions.find(_.isDefined).flatten
          }
      }
    }

    solve(rows, List())
  }

  def printSolution(X: Int, Y: Int, sol: Solution) = {
    for (i <- 0 until X) {
      print("|")
      for (j <- 0 until Y) {
        if (sol.contains((i, j)))
          print("X|")
        else
          print("_|")
      }
      println()
    }
  }
}