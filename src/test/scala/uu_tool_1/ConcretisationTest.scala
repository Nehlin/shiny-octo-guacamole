import org.scalatest.{Matchers, FlatSpec}
import uu_tool_1.Concretisation

import scala.collection.mutable.ArrayBuffer

class ConcretisationTest extends FlatSpec with Matchers {

  "Concretisation" should "work using naive implementation" in {
    Concretisation.naive(Set(ArrayBuffer(1)), 1) should be (Set(ArrayBuffer(1, 1)))

    Concretisation.naive(Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(2, 3, 4)
    ), 3) should be (Set())

    Concretisation.naive(Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(1, 2, 4),
      ArrayBuffer(1, 3, 4),
      ArrayBuffer(2, 3, 4)
    ), 3) should be (Set(ArrayBuffer(1, 2, 3, 4)))

    Concretisation.naive(Set(
      ArrayBuffer(1, 2),
      ArrayBuffer(1, 3),
      ArrayBuffer(1, 4),
      ArrayBuffer(2, 3),
      ArrayBuffer(2, 4),
      ArrayBuffer(3, 4)
    ), 2) should be (Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(1, 2, 4),
      ArrayBuffer(1, 3, 4),
      ArrayBuffer(2, 3, 4)
    ))
  }
}