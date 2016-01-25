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

  "Configurations" should "have increasing identifiers" in {
    Configuration.makeIdentifier(Vector(0), 10) should be(0)
    Configuration.makeIdentifier(Vector(1), 10) should be(1)
    Configuration.makeIdentifier(Vector(9), 10) should be(9)
    Configuration.makeIdentifier(Vector(0, 0), 10) should be(10)
    Configuration.makeIdentifier(Vector(5, 5), 10) should be(65)
    Configuration.makeIdentifier(Vector(9, 9), 10) should be(109)
    Configuration.makeIdentifier(Vector(0, 0, 0), 10) should be(110)
    Configuration.makeIdentifier(Vector(0, 0, 1), 10) should be(111)
  }

  "Configurations" should "have identifiers when skipping a state" in {
    Configuration.makeIdentifierSkip(Vector(3, 7), 10, 0) should be(Configuration.makeIdentifier(Vector(7), 10))
    Configuration.makeIdentifierSkip(Vector(3, 7), 10, 1) should be(Configuration.makeIdentifier(Vector(3), 10))
    Configuration.makeIdentifierSkip(Vector(3, 7), 10, 1) should be(Configuration.makeIdentifier(Vector(3), 10))
    Configuration.makeIdentifierSkip(Vector(2, 4, 8), 10, 0) should be(Configuration.makeIdentifier(Vector(4, 8), 10))
    Configuration.makeIdentifierSkip(Vector(2, 4, 8), 10, 1) should be(Configuration.makeIdentifier(Vector(2, 8), 10))
    Configuration.makeIdentifierSkip(Vector(2, 4, 8), 10, 2) should be(Configuration.makeIdentifier(Vector(2, 4), 10))
  }

  "Configurations" should "have identifiers with fixed starts" in {
    Configuration.makeIdentifierFixedStart(1, 2, Vector(), 10) should be(Configuration.makeIdentifier(Vector(1, 2), 10))
    Configuration.makeIdentifierFixedStart(3, 9, Vector(2), 10) should be(Configuration.makeIdentifier(Vector(3, 9, 2), 10))
  }
}