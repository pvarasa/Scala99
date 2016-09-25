import org.scalatest.FunSuite

class ListTests extends FunSuite {
  val example = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val lists = new Lists

  test("pack") {
    assert(lists.pack(example) ==
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("encode decode") {
    assert(example == lists.decode(lists.encode(example)))
  }

  test("duplicate") {
    assert(lists.duplicateN(3, List('a, 'b, 'c)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c))
  }

  test("combinations") {
    val t = List('a, 'b, 'c, 'd, 'e)
    assert(t.combinations(3).toList == lists.combinations(3, t))
  }

  test("group") {
    val t = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val groups = List(2, 2, 5)
    val result = lists.group(groups, t)

    for (g <- result) {
      g.zip(groups).foreach({ case (list, len) => assert(list.length == len) })
      assert(g.distinct.length == g.length)
    }
  }
}
