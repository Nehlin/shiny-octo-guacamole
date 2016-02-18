import org.scalatest._
import uu_tool_1.{Unrestricted, Universal, Existential, Both, Left, Right}
import scala.collection.mutable.ArrayBuffer

class RuleTest extends FlatSpec with Matchers {

  val existentialLeft = new Existential(1, 2, Set(2, 3, 4), Left)
  val existentialRight = new Existential(1, 2, Set(0, 1, 2), Right)
  val existentialPass = new Existential(1, 2, Set(1, 2), Both)
  val existentialFail = new Existential(1, 2, Set(2), Both)
  val universalLeft = new Universal(1, 2, Set(0, 1), Left)
  val universalRight = new Universal(1, 2, Set(3, 4), Right)
  val universal = new Universal(1, 2, Set(0, 1, 3, 4), Both)
  val unrestricted = new Unrestricted(1, 2)
  val configuration = ArrayBuffer(0, 1, 2, 3, 4)

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

  // 1, 2 should exist on either side of index
  "An existential quantifier" should "evaluate to true iff there is at least one required state at any side of index 1" in {
    existentialPass.testQuantifier(configuration, 0) should be (true)
    existentialPass.testQuantifier(configuration, 1) should be (true)
    existentialPass.testQuantifier(configuration, 2) should be (true)
    existentialPass.testQuantifier(configuration, 3) should be (true)
    existentialPass.testQuantifier(configuration, 4) should be (true)
  }

  // 2 should exist on either side of index
  "An existential quantifier" should "evaluate to true iff there is at least one required state at any side of index 2" in {
    existentialFail.testQuantifier(configuration, 0) should be (true)
    existentialFail.testQuantifier(configuration, 1) should be (true)
    existentialFail.testQuantifier(configuration, 2) should be (false)
    existentialFail.testQuantifier(configuration, 3) should be (true)
    existentialFail.testQuantifier(configuration, 4) should be (true)
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

  // All states to either side of index should be 0, 1, 3, 4
  "A universal quantifier" should "evaluate to true iff all states on both sides of index are of required type" in {
    universal.testQuantifier(configuration, 0) should be (false)
    universal.testQuantifier(configuration, 1) should be (false)
    universal.testQuantifier(configuration, 2) should be (true)
    universal.testQuantifier(configuration, 3) should be (false)
    universal.testQuantifier(configuration, 4) should be (false)
  }

  "A rule" should "evaluate to true iff source of rule is equal to state in configuration" in {
    unrestricted.testFromState(configuration, 0) should be (false)
    unrestricted.testFromState(configuration, 1) should be (true)
    unrestricted.testFromState(configuration, 2) should be (false)
  }
}


