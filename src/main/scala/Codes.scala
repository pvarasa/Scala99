import scala.collection.mutable
import scala.math.Ordering

class Codes {
  type HuffTree[A] = Tree[(Option[A], Double)]
  type HuffNode[A] = Node[(Option[A], Double)]

  def p[A](node: HuffNode[A]) = node.value._2

  // P50
  def huffman[A](freqs: List[(A, Int)])(implicit ord: Ordering[A]): List[(A, String)] = {
    val queue = new mutable.PriorityQueue[HuffNode[A]]()(Ordering.by(p))
    val total: Double = freqs.map(_._2).sum
    for ((symbol, freq) <- freqs)
      // transform frequency to priority= -probability
      queue += Node((Some(symbol), -freq/total), End, End)

    while (queue.size >= 2) {
      val (n1, n2) = (queue.dequeue(), queue.dequeue())
      queue += Node((None, p(n1) + p(n2)), n1, n2)
    }

    def walk(tree: HuffTree[A], acc: String = ""): List[(A, String)] = tree match {
      case Node((Some(s), _), _, _) => List((s, acc))
      case Node((None   , _), a, b) => walk(a, acc+"0") ++ walk(b, acc+"1")
    }

    walk(queue.dequeue()).sortBy(_._1)
  }
}

