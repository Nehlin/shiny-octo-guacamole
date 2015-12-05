import org.scalatest._

class SubWordTest extends FlatSpec with Matchers {
  val l1 = List(1, 2, 3, 4, 5)
  val l2 = List(2, 4)
  val l3 = List(1)
  val l4 = List(1, 5)
  val l5 = List(6)
  val l6 = List(2, 1)
  val l7 = List()

  "Subwords" should "be testable" in {
    SubWord.test(l1, l1) should be (true)
    SubWord.test(l1, l2) should be (true)
    SubWord.test(l2, l1) should be (false)
    SubWord.test(l1, l3) should be (true)
    SubWord.test(l1, l5) should be (false)
    SubWord.test(l1, l6) should be (false)
    SubWord.test(l1, l7) should be (true)
    SubWord.test(l7, l7) should be (true)
    SubWord.test(l7, l3) should be (false)
  }
}
