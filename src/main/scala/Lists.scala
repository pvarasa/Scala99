
class Lists {
  // P09
  def pack[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case x => x.span(x.head.==) match { case (m, nm) => m :: pack(nm) }
  }

  // P10
  def encode[A](list: List[A]): List[(Int, A)] = pack(list).map(x => (x.length, x.head))

  // P11
  def encodeModified[A](list: List[A]): List[Any] = encode(list).map({
    case (1, x) => x
    case p => p
  })

  // P12
  def decode[A](list: List[(Int, A)]): List[A] = list.flatMap({case (n, s) => List.fill(n)(s)})

  // P15
  def duplicateN[A](n: Int, list: List[A]): List[A] = list.flatMap(List.fill(n)(_))

  // P26
  def combinations[A](n: Int, list: List[A]): List[List[A]] = list match {
    case h :: t =>
      if (n==1) list.map(List(_))
      else combinations(n-1, t).map(h :: _) ++ combinations(n, t)
    case empty => List()
  }

  // P27
  def group[A](q: List[Int], gr: List[A]): List[List[List[A]]] = q match {
    case n :: xs => combinations(n, gr).flatMap(c =>
      if (xs == Nil) List(List(c))
      else group(xs, gr.filterNot(c.contains)).map(c::_))
    case empty => List()
  }

  // P28
  def lsort[A](list: List[List[A]]): List[List[A]] = list.sortBy(_.length)
}
