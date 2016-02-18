import org.scalatest._
import uu_tool_1.Views
import scala.collection.mutable.{ArrayBuffer, Set => MSet}

class ViewsTest extends FlatSpec with Matchers {
  var c1 = ArrayBuffer(0)
  var c2 = ArrayBuffer(1, 2, 3)
  var c3 = ArrayBuffer(4, 4, 4)
  var c4 = ArrayBuffer(6, 7, 8, 9, 10)
  var c5 = ArrayBuffer(1, 4, 3, 4)

  "Views" should "be created from configurations of size 1" in {
    Views.fromConfiguration(c1, 0) should be (Set())
    Views.fromConfiguration(c1, 1) should be (Set(ArrayBuffer(0)))
  }

  "Views" should "be created from configurations of larger sizes" in {
    Views.fromConfiguration(c2, 0) should be (Set())
    Views.fromConfiguration(c2, 1) should be (Set(ArrayBuffer(1), ArrayBuffer(2), ArrayBuffer(3)))
    Views.fromConfiguration(c2, 2) should be (Set(ArrayBuffer(1, 2), ArrayBuffer(1, 3), ArrayBuffer(2, 3)))
    Views.fromConfiguration(c2, 3) should be (Set(ArrayBuffer(1, 2, 3)))
    Views.fromConfiguration(c4, 3) should be (Set(
      ArrayBuffer(6, 7, 8),
      ArrayBuffer(6, 7, 9),
      ArrayBuffer(6, 7, 10),
      ArrayBuffer(6, 8, 9),
      ArrayBuffer(6, 8, 10),
      ArrayBuffer(6, 9, 10),
      ArrayBuffer(7, 8, 9),
      ArrayBuffer(7, 8, 10),
      ArrayBuffer(7, 9, 10),
      ArrayBuffer(8, 9, 10)
    ))
  }

  "Views" should "not create duplicates" in {
    Views.fromConfiguration(c3, 2) should be (Set(ArrayBuffer(4, 4)))
    Views.fromConfiguration(c5, 2) should be (Set(
      ArrayBuffer(1, 4),
      ArrayBuffer(1, 3),
      ArrayBuffer(4, 3),
      ArrayBuffer(4, 4),
      ArrayBuffer(3, 4)
    ))
  }

  "Views" should "be created from multiple configurations" in {
    Views.fromConfigurations(Set(c4, c5), 4) should be (Set(
      ArrayBuffer(7, 8, 9, 10),
      ArrayBuffer(6, 8, 9, 10),
      ArrayBuffer(6, 7, 9, 10),
      ArrayBuffer(6, 7, 8, 10),
      ArrayBuffer(6, 7, 8, 9),
      ArrayBuffer(1, 4, 3, 4)
    ))
    Views.fromConfigurations(Set(c1, c3, c4), 1) should be (Set(
      ArrayBuffer(0),
      ArrayBuffer(4),
      ArrayBuffer(6),
      ArrayBuffer(7),
      ArrayBuffer(8),
      ArrayBuffer(9),
      ArrayBuffer(10)
    ))
  }

  "Views" should "be created of fix length" in {
    Views.fromConfigurationFixed(c1, None) should be (Set(ArrayBuffer()))
    Views.fromConfigurationFixed(c2, None) should be (Set(
      ArrayBuffer(1, 2),
      ArrayBuffer(1, 3),
      ArrayBuffer(2, 3)
    ))
    Views.fromConfigurationFixed(c4, None) should be (Set(
      ArrayBuffer(7, 8, 9, 10),
      ArrayBuffer(6, 8, 9, 10),
      ArrayBuffer(6, 7, 9, 10),
      ArrayBuffer(6, 7, 8, 10),
      ArrayBuffer(6, 7, 8, 9)
    ))
  }

  val o:Option[Set[ArrayBuffer[Int]]] = Some(Set(ArrayBuffer(1, 3)))

  "Views" should "ignore existing views" in {
    Views.fromConfigurationFixed(c2, Some(MSet(ArrayBuffer(1, 3)))) should be (Set(
      ArrayBuffer(1, 2),
      ArrayBuffer(2, 3)
    ))
    Views.fromConfigurationFixed(c4, Some(MSet(
      ArrayBuffer(6, 8, 9, 10),
      ArrayBuffer(6, 7, 8, 9)
    ))) should be (Set(
      ArrayBuffer(7, 8, 9, 10),
      ArrayBuffer(6, 7, 9, 10),
      ArrayBuffer(6, 7, 8, 10)
    ))
  }
}