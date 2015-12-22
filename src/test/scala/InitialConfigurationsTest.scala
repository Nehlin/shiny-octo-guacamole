import org.scalatest._

class InitialConfigurationsTest extends FlatSpec with Matchers {

  val nameToInternal = Map("Red" -> 0, "Green" -> 1)
  val inputString1 = "Red"
  val inputString2 = "Green"

  val i1 = new InitialConfigurations(inputString1, nameToInternal)
  val i2 = new InitialConfigurations(inputString2, nameToInternal)

  "Initial configurations" should "be possible to generate from a single repeating variable" in {
    i1.make(0) should be(Vector())
    i1.make(1) should be(Vector(0))
    i2.make(2) should be(Vector(1, 1))
  }

}