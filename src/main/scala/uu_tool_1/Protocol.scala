package uu_tool_1

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * A protocol parses and stores the states, initial state, the bad state and the rules of a protocol.
 *
 * After successfully parsing the input file, a protocol will let us access the following:
 * stateNames - list of all state names
 * alphabetLength - number of available states
 * nameToInternal - a mapping from state names to the internal integer representation of states
 * internalToName - a mapping from the internal integer representation of states to state names
 * rules - the set of rules in the protocol
 * initialConfiguration(len) - a method that when called creates an initial configuration of length = len
 * badConfiguration - the bad configuration
 *
 *
 * @param fileName file name of file containing a protocol. For more information about the protocol,
 *                 please refer to the html documentation and to the example protocol, burns.txt
 */
class Protocol(fileName:String) {

  private val rawLines = Source.fromFile(fileName).getLines().toList

  // Remove any whitespace in the beginning or end of the line and any multiple whitespaces
  private val trimmedLines = rawLines.map(_.trim).map(line => line.replaceAll(" +", " "))
  // Remove all empty and comment lines.
  private val lines = trimmedLines.filter(l => !l.startsWith("%")).filter(l => l != "")

  private val statesIndex = lines.indexOf("States:")
  private val initialConfigurationsIndex = lines.indexOf("Initial configurations:")
  private val badConfigurationIndex = lines.indexOf("Bad configuration:")
  private val rulesIndex = lines.indexOf("Rules:")

  if (statesIndex == -1 ||
    initialConfigurationsIndex == -1 ||
    badConfigurationIndex == -1 ||
    rulesIndex == -1) {

    throw new Exception("Cannot parse input file: " + fileName)
  }

  private val stateLines = lines.slice(statesIndex + 1, initialConfigurationsIndex)
  private val initialConfigurationLine = lines(initialConfigurationsIndex + 1)
  private val badConfigurationLine = lines(badConfigurationIndex + 1)
  private val ruleLines = lines.drop(rulesIndex + 1)

  val stateNames = stateLines
  val alphabetLength = stateNames.length
  val nameToInternal = stateNames.zipWithIndex.toMap
  val internalToName = stateNames.zipWithIndex.map(_.swap).toMap

  private def parseBadConfiguration(badConfigString: String): ArrayBuffer[Int] = {
    val stateNames = badConfigString.split(",").map(_.trim)
    stateNames.map(nameToInternal(_)).to[ArrayBuffer]
  }

  private def parseRule(ruleString: String): Rule = {
    val implicationSplit = ruleString.split("->")
    val lhs = implicationSplit.head.trim
    val rhs = implicationSplit.tail.head.trim
    val to = nameToInternal(rhs)
    // If left hand side contains a single word, it is a unrestricted rule
    if (!lhs.contains(" ")) {
      val from = nameToInternal(lhs)
      Unrestricted(from, to)
      // Otherwise it is an existential or universal rule
    } else {
      val openingParenIndex = lhs.indexOf("(")
      val closingParenIndex = lhs.indexOf(")")

      val stateNames = lhs.slice(openingParenIndex + 1, closingParenIndex).replaceAll("\\s", "").split(",")
      val states = stateNames.map(nameToInternal(_)).toSet

      val beforeParen = lhs.take(openingParenIndex).trim.replaceAll("\\s+", " ")
      val beforeParenSplit = beforeParen.split(" ")

      val fromName = beforeParenSplit(0)
      val quantifierName = beforeParenSplit(1)

      val from = nameToInternal(fromName)
      val isExistential = quantifierName == "E"

      val side = if (beforeParenSplit.length == 2) {
        Both
      } else {
        val sideName = beforeParenSplit(2)
        if (sideName == "L") {
          Left
        } else {
          Right
        }
      }

      if (isExistential) {
        Existential(from, to, states, side)
      } else {
        Universal(from, to, states, side)
      }
    }
  }

  private val initialConfigurationState = nameToInternal(initialConfigurationLine)

  val rules = ruleLines.map(ruleString => parseRule(ruleString)).toSet
  def initialConfiguration(length: Int): ArrayBuffer[Int] =
    ArrayBuffer.fill(initialConfigurationState)(length)
  val badConfiguration = parseBadConfiguration(badConfigurationLine)
}