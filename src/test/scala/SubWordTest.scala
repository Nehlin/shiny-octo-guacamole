import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class SubWordTest extends FlatSpec with Matchers {
  val l1 = ArrayBuffer(1, 2, 3, 4, 5)
  val l2 = ArrayBuffer(2, 4)
  val l3 = ArrayBuffer(1)
  val l4 = ArrayBuffer(1, 5)
  val l5 = ArrayBuffer(6)
  val l6 = ArrayBuffer(2, 1)
  val l7 = ArrayBuffer[Int]()

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
