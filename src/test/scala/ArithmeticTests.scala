import org.scalatest.FunSuite

class ArithmeticTests extends FunSuite {
  val arith = new Arithmetic

  test("Goldbach") {
    assert(arith.goldbach(28) == (5, 23))
  }

}
