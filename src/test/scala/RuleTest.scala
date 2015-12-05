import org.scalatest._

class RuleTest extends FlatSpec with Matchers {

  val existentialLeft = new Existential(1, 2, Set(2, 3, 4), Left)
  val existentialRight = new Existential(1, 2, Set(0, 1, 2), Right)
  val universalLeft = new Universal(1, 2, Set(0, 1), Left)
  val universalRight = new Universal(1, 2, Set(3, 4), Right)
  val unrestricted = new Unrestricted(1, 2)
  val configuration = new Configuration(Vector(0, 1, 2, 3, 4))

  // 2, 3, 4 should exist on left
  "An existential quantifier" should "evaluate to true iff there is at least one required state at the left side of index" in {
    existentialLeft.testQuantifier(configuration, 0) should be (false)
    existentialLeft.testQuantifier(configuration, 1) should be (false)
    existentialLeft.testQuantifier(configuration, 2) should be (false)
    existentialLeft.testQuantifier(configuration, 3) should be (true)
    existentialLeft.testQuantifier(configuration, 4) should be (true)
  }

  // 0, 1, 2 should exist on right
  "An existential quantifier" should "evaluate to true iff there is at least one required state at the right side of index" in {
    existentialRight.testQuantifier(configuration, 0) should be (true)
    existentialRight.testQuantifier(configuration, 1) should be (true)
    existentialRight.testQuantifier(configuration, 2) should be (false)
    existentialRight.testQuantifier(configuration, 3) should be (false)
    existentialRight.testQuantifier(configuration, 4) should be (false)
  }

  // All states to left should be 0, 1
  "A universal quantifier" should "evaluate to true iff all states on the left side of index are of required type" in {
    universalLeft.testQuantifier(configuration, 0) should be (true)
    universalLeft.testQuantifier(configuration, 1) should be (true)
    universalLeft.testQuantifier(configuration, 2) should be (true)
    universalLeft.testQuantifier(configuration, 3) should be (false)
    universalLeft.testQuantifier(configuration, 4) should be (false)
  }

  // All states to right should be 3, 4
  "A universal quantifier" should "evaluate to true iff all states on the right side of index are of required type" in {
    universalRight.testQuantifier(configuration, 0) should be (false)
    universalRight.testQuantifier(configuration, 1) should be (false)
    universalRight.testQuantifier(configuration, 2) should be (true)
    universalRight.testQuantifier(configuration, 3) should be (true)
    universalRight.testQuantifier(configuration, 4) should be (true)
  }

  "A rule" should "evaluate to true iff source of rule is equal to state in configuration" in {
    unrestricted.testFromState(configuration, 0) should be (false)
    unrestricted.testFromState(configuration, 1) should be (true)
    unrestricted.testFromState(configuration, 2) should be (false)
  }
}


