import org.scalatest._

class ConfigurationTest extends FlatSpec with Matchers {

  val configuration = new Configuration(Vector(0, 1, 2, 3, 4))

  "Configurations" should "know leftOf and rightOf" in {
    configuration.leftOf(0).force should be(Vector())
    configuration.leftOf(2).force should be(Vector(0, 1))
    configuration.leftOf(4).force should be(Vector(0, 1, 2, 3))
    configuration.rightOf(0).force should be(Vector(1, 2, 3, 4))
    configuration.rightOf(2).force should be(Vector(3, 4))
    configuration.rightOf(4).force should be(Vector())
  }

  "Configurations" should "know atIndex" in {
    configuration.aAtIndex(0) should be(0)
    configuration.aAtIndex(4) should be(4)
  }

  "Configurations" should "know update" in {
    configuration.updated(0, 1).states should be(Vector(1, 1, 2, 3, 4))
  }

  val c1 = new Configuration(Vector(1, 1))
  val c2 = new Configuration(Vector(1, 2))
  val c3 = new Configuration(Vector(5))
  val c4 = new Configuration(Vector(0, 2))
  "Configurations" should "have an ordering" in {
    c1.compare(c2) should be(-1)
    c1.compare(c1) should be(0)
    c1.compare(c3) should be(1)
    c1.compare(c4) should be(1)
  }
}