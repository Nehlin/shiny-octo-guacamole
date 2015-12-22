import org.scalatest._

class ConfigurationTest extends FlatSpec with Matchers {

  val c = Vector(0, 1, 2, 3, 4)

  "Configurations" should "know leftOf and rightOf" in {
    Configuration.leftOf(c, 0).force should be(Vector())
    Configuration.leftOf(c, 2).force should be(Vector(0, 1))
    Configuration.leftOf(c, 4).force should be(Vector(0, 1, 2, 3))
    Configuration.rightOf(c, 0).force should be(Vector(1, 2, 3, 4))
    Configuration.rightOf(c, 2).force should be(Vector(3, 4))
    Configuration.rightOf(c, 4).force should be(Vector())
  }

  val c1 = Vector(1, 1)
  val c2 = Vector(1, 2)
  val c3 = Vector(5)
  val c4 = Vector(0, 2)
  "Configurations" should "have an ordering" in {
    Configuration.compare(c1, c2) should be(-1)
    Configuration.compare(c1, c1) should be(0)
    Configuration.compare(c1, c3) should be(1)
    Configuration.compare(c1, c4) should be(1)
  }
}