import org.scalatest.{Matchers, FlatSpec}
import uu_tool_1.{Views, Concretisation}

import scala.collection.mutable.ArrayBuffer

class ConcretisationTest extends FlatSpec with Matchers {

  /**
   * Concretising views should generate the new view (1, 2, 2, 3)
   */
  val views = Views.fromConfigurationsFixed(Set(
    ArrayBuffer(1, 2, 3, 4),
    ArrayBuffer(2, 2, 3, 5),
    ArrayBuffer(1, 2, 4, 5),
    ArrayBuffer(1, 2, 2, 5)
  ), None)

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

    Concretisation.naive(views, 3) should be (Set(
      ArrayBuffer(1, 2, 2, 3),
      ArrayBuffer(1, 2, 2, 5),
      ArrayBuffer(1, 2, 3, 4),
      ArrayBuffer(1, 2, 4, 5),
      ArrayBuffer(2, 2, 3, 5)
    ))
  }

  "Concretisation" should "work using faster implementation" in {
    Concretisation.make(Set(ArrayBuffer(1)), Set()) should be (Set(ArrayBuffer(1, 1)))

    Concretisation.make(Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(2, 3, 4)
    ), Set()) should be (Set())

    Concretisation.make(Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(1, 2, 4),
      ArrayBuffer(1, 3, 4),
      ArrayBuffer(2, 3, 4)
    ), Set()) should be (Set(ArrayBuffer(1, 2, 3, 4)))

    Concretisation.make(Set(
      ArrayBuffer(1, 2),
      ArrayBuffer(1, 3),
      ArrayBuffer(1, 4),
      ArrayBuffer(2, 3),
      ArrayBuffer(2, 4),
      ArrayBuffer(3, 4)
    ), Set()) should be (Set(
      ArrayBuffer(1, 2, 3),
      ArrayBuffer(1, 2, 4),
      ArrayBuffer(1, 3, 4),
      ArrayBuffer(2, 3, 4)
    ))

    Concretisation.naive(views, 3) should be (Set(
      ArrayBuffer(1, 2, 2, 3),
      ArrayBuffer(1, 2, 2, 5),
      ArrayBuffer(1, 2, 3, 4),
      ArrayBuffer(1, 2, 4, 5),
      ArrayBuffer(2, 2, 3, 5)
    ))
  }
}