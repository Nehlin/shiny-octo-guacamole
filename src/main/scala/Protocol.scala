import scala.io.Source

class Protocol(fileName:String) {

  // TODO: move parsing to separate class

  private val rawLines = Source.fromFile(fileName).getLines().toList
  private val trimmedLines = rawLines.map(_.trim)
  private val lines = trimmedLines.filter(l => !l.startsWith("%")).filter(l => l != "")

  private val statesIndex = lines.indexOf("States:")
  private val initialConfigurationsIndex = lines.indexOf("Initial configurations:")
  private val rulesIndex = lines.indexOf("Rules:")

  if (statesIndex == -1 || initialConfigurationsIndex == -1 || rulesIndex == -1) {
    throw new Exception("Cannot parse input file: " + fileName)
  }

  private val stateLines = lines.slice(statesIndex + 1, initialConfigurationsIndex)
  private val initialConfigurationLine = lines.slice(initialConfigurationsIndex + 1, rulesIndex).head
  private val ruleLines = lines.drop(rulesIndex + 1)

  val stateNames = stateLines
  val alphabetLength = stateNames.length
  val nameToInternal = stateNames.zipWithIndex.toMap
  val internalToName = stateNames.zipWithIndex.map(_.swap).toMap

  def parseRule(ruleString: String): Rule = {
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
      val sideName = beforeParenSplit(2)

      val from = nameToInternal(fromName)
      val isExistential = quantifierName == "E"
      val side = if (sideName == "L") {
        Left
      } else {
        Right
      }

      if (isExistential) {
        Existential(from, to, states, side)
      } else {
        Universal(from, to, states, side)
      }
    }
  }

  val rules = ruleLines.map(ruleString => parseRule(ruleString)).toSet
  val initialConfiguration = new InitialConfigurations(initialConfigurationLine, nameToInternal)

}