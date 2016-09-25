class Arithmetic {
  // P39
  def listPrimesinRange(range: Range): List[Int] =
    range.filter(n => (2 until n).forall(n % _ != 0)).toList

  // P40
  def goldbach(n: Int): (Int, Int) =
    listPrimesinRange(2 to n).combinations(2).find(_.sum == n) match {
      case Some(n1 :: n2 :: _) => (n1, n2)
      case other => throw new RuntimeException("Fields medal!")
    }
}
