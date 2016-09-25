import org.scalatest.FunSuite

class CodeTests extends FunSuite {
  val codes = new Codes

  test("Huffman code") {
    val huffmanExample = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    assert(codes.huffman(huffmanExample) ==
      List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")))
  }
}
