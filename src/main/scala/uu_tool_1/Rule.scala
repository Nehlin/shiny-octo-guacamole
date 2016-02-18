package uu_tool_1

import scala.collection.mutable.ArrayBuffer

abstract class Side
case object Left extends Side
case object Right extends Side
case object Both extends Side

/**
 * Super class for all rules. A rule is a transition from one configuration to another.
 *
 * The key concepts of rules are testing and execution.
 *
 * A rule is tested for a certain state in a configuration to see if that state satisfies
 * the condition(s) of the rule. A rule can only be executed if the test evaluates to true
 *
 * Executing a rule means creating a new configuration where one state == from is updated to
 * to.
 *
 * @param f from state in the original transition
 * @param t to state in the new configuration
 */
abstract class Rule(f: Int, t: Int) extends Ordered[Rule] {
  val from = f
  val to = t

  /**
   * Used by toString. Creates a string from source- and target-string plus an optional condition
   *
   * @param fromString source state string
   * @param toString target state string
   * @param condition optional condition (for quantifier rules)
   * @return the following string representation: fromString -[condition]-> toString
   */
  def stringFunBasic(fromString:String, toString:String, condition:Option[String]): String = {
    val conditionString = condition match {
      case None => " -> "
      case Some(c) => " -" + c + "-> "
    }
    fromString + conditionString + toString
  }

  /**
   * Used by toString. Creates a string using the internal (integer) representation of the states.
   * Used to provide a toString that does not require a translation from internal state to
   * original name. Sometimes usable, but the translated version is usually preferred.
   *
   * @param fromInternal internal representation of from-state
   * @param toInternal internal representation of to-state
   * @param condition optional condition (for quantifier rules)
   * @return the following string representation: fromInternal -[condition]-> toInternal
   */
  def stringFunInternal(fromInternal: Int,
                  toInternal: Int,
                   condition: Option[String]): String = {

    stringFunBasic(fromInternal.toString, toInternal.toString, condition)
  }

  /**
   * Used by overloaded toString. Creates a string using the original names of the states.
   *
   * @param internalToName translation map from internal state name to original name
   * @param fromInternal internal representation of from-state
   * @param toInternal internal representation of to-state
   * @param condition optional condition (for quantifier rules)
   * @return the following string representation where fromName and toName are the
   *         corresponding names of fromInternal and toInternal specified by internalToName:
   *         fromName -[condition]-> toName
   */
  def stringFunTranslated(internalToName: Map[Int, String],
                  fromInternal: Int,
                    toInternal: Int,
                     condition: Option[String]): String = {

    val fromString = internalToName(fromInternal)
    val toString = internalToName(toInternal)
    stringFunBasic(fromString, toString, condition)
  }

  /**
   * Abstract method to create a string using a translation-map. See stringFunTranslated
   * for more details.
   *
   * @param internalToName translation map from internal state name to original name
   * @return string representation of the rule, using original names
   */
  def toString(internalToName: Map[Int, String]): String

  /**
   * Tests to see if the current state of a configuration equals the from-state of the rule.
   * This is a necessary condition for all rules
   *
   * @param configuration test configuration
   * @param index index of state to test in configuration
   * @return true iff configuration(index) == from
   */
  def testFromState(configuration: ArrayBuffer[Int], index: Int): Boolean = {
    configuration(index) == from
  }

  /**
   * Abstract method that tests a state in a configuration.
   * @param configuration test configuration
   * @param index index of state to test in configuration
   * @return true iff all conditions for the rule evaluate to true.
   */
  def test(configuration: ArrayBuffer[Int], index: Int): Boolean


  /**
   * If the rule test evaluates to true, a new configuration is created that is the old
   * configuration with the state at index updated from the from-state of the rule to its
   * to-state.
   *
   * @param configuration original configuration
   * @param index index of state to test and update
   * @return Some(updated configuration) if the rule test is successful, None otherwise.
   */
  def testAndExecute(configuration: ArrayBuffer[Int], index: Int): Option[ArrayBuffer[Int]] = {
    if (test(configuration, index)) {
      Some(configuration.updated(index, to))
    } else {
      None
    }
  }

  /**
   * Compares two rules, for sorting purposes. This only considers states, not rule-type.
   * Only used for slightly prettier .dot-generation.
   *
   * @param otherRule rule to compare to this rule
   * @return -1 if this rule < otherRule, 0 if they are equal, 1 otherwise.
   */
  def compare(otherRule: Rule): Int = {
    val compareFrom = from.compareTo(otherRule.from)
    if (compareFrom != 0) {
      compareFrom
    } else {
      to.compareTo(otherRule.to)
    }
  }
}

/**
 * An unrestricted rule is always valid, as long as the source state is correct.
 *
 * @param f from state in the original transition
 * @param t to state in the new configuration
 */
case class Unrestricted(f: Int, t: Int) extends Rule(f, t) {
  override def toString = stringFunInternal(from, to, None)
  def toString(internalToName: Map[Int, String]) = {
    stringFunTranslated(internalToName, from, to, None)
  }

  /**
   * A unrestricted rule is valid whenever the from-state is correct
   *
   * @param configuration test configuration
   * @param index index of state to test in configuration
   * @return true iff the test state is equal to the from-state.
   */
  def test(configuration: ArrayBuffer[Int], index: Int): Boolean = testFromState(configuration, index)
}

/**
 * A quantifier rule is a rule that has a quantifier condition as well as a from-state condition.
 *
 * @param f from state in the original transition
 * @param t to state in the new configuration
 * @param st states present in the quantifier condition
 * @param si side that the quantifier condition applies to
 * @param qs string representation of the quantifier
 */
abstract class QuantifierRule(f: Int, t: Int, st: Set[Int], si: Side, qs: String, itv: Boolean) extends Rule(f, t) {
  val quantifierString = qs
  val conditionStates = st
  val side = si
  val initialTestValue = itv

  private def statesStringBasic(stateStrings: Set[String]): String = {
    val sideString = if (side == Left) ".L" else if (side == Right) ".R" else ""

    quantifierString + sideString + "(" + stateStrings.toList.sorted.mkString(",") + ")"
  }

  private def statesStringTranslated(internalToName: Map[Int, String]): String = {
    statesStringBasic(conditionStates.map(internalToName(_)))
  }

  private def statesString: String = statesStringBasic(conditionStates.map(_.toString))

  override def toString = stringFunInternal(from, to, Some(statesString))

  def toString(internalToName: Map[Int, String]): String =
    stringFunTranslated(internalToName, from, to, Some(statesStringTranslated(internalToName)))

  /**
   * This method will be run as a fold on all states to the specified side of the
   * rule. The initial value for the accumulator is initialTestValue. This is where
   * the rules are evaluated
   *
   * @param res accumulator value
   * @param state current state in the folding
   * @return the result of evaluating the rule for all states on the specified side.
   */
  def testFunction(res: Boolean, state: Int): Boolean

  /**
   * Test for the quantifier condition.
   *
   * @param configuration test configuration
   * @param index index of state in configuration to test from
   * @return true iff the quantifier condition is satisfied
   */
  def testQuantifier(configuration: ArrayBuffer[Int], index: Int): Boolean = {
    side match {
      case Left => Configuration.leftOf[Boolean](configuration, index, initialTestValue, testFunction)
      case Right => Configuration.rightOf[Boolean](configuration, index, initialTestValue, testFunction)
      case Both => Configuration.bothSidesOf[Boolean](configuration, index, initialTestValue, testFunction)
    }
  }

  /**
   * A quantifier rule requires both the from-state test and the quantifier-test to be valid
   * in order to be valid
   *
   * @param configuration test configuration
   * @param index index of state to test in configuration
   * @return true iff all conditions for the rule evaluate to true.
   */
  def test(configuration: ArrayBuffer[Int], index: Int): Boolean = {
    testFromState(configuration, index) && testQuantifier(configuration, index)
  }
}

/**
 * An existential rule is valid if there is at least one state from st at the specified side
 *
 * @param f from state in the original transition
 * @param t to state in the new configuration
 * @param st states present in the quantifier condition
 * @param si side that the quantifier condition applies to
 */
case class Existential(f: Int, t: Int, st: Set[Int], si: Side)
  extends QuantifierRule(f, t, st, si, "\u2203", false) {

  def testFunction(res: Boolean, state: Int): Boolean = {
    if (res) {
      res
    } else {
      conditionStates.contains(state)
    }
  }
}

/**
 * A universal rule is valid iff every state from st is present on the specified side.
 *
 * @param f from state in the original transition
 * @param t to state in the new configuration
 * @param st states present in the quantifier condition
 * @param si side that the quantifier condition applies to
 */
case class Universal(f: Int, t: Int, st: Set[Int], si: Side)
  extends QuantifierRule(f, t, st, si, "\u2200", true) {

  def testFunction(res: Boolean, state: Int): Boolean = {
    if (!res) {
      res
    } else {
      conditionStates.contains(state)
    }
  }
}