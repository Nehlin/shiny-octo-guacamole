import scala.collection.SeqView

abstract class Side
case object Left extends Side
case object Right extends Side

abstract class Rule(f: Int, t: Int) extends Ordered[Rule] {
  val from = f
  val to = t
  def stringFun(fromString:String, toString:String, condition:Option[String]): String = {
    val conditionString = condition match {
      case None => " -> "
      case Some(c) => " -" + c + "-> "
    }
    fromString + conditionString + toString
  }
  def stringFun(fromInternal: Int,
                        toInternal: Int,
                        condition: Option[String]): String = {
    stringFun(fromInternal.toString, toInternal.toString, condition)
  }
  def stringFun(internalToName: Map[Int, String],
                fromInternal: Int,
                toInternal: Int,
                condition: Option[String]): String = {

    val fromString = internalToName(fromInternal)
    val toString = internalToName(toInternal)
    stringFun(fromString, toString, condition)
  }
  def toString(internalToName: Map[Int, String]): String




  def testFromState(configuration: Vector[Int], index: Int): Boolean = {
    configuration(index) == from
  }
  def test(configuration: Vector[Int], index: Int): Boolean
  def execute(configuration: Vector[Int], index: Int): Vector[Int] = {
    configuration.updated(index, to)
  }
  def testAndExecute(configuration: Vector[Int], index: Int): Option[Vector[Int]] = {
    if (test(configuration, index)) {
      Some(execute(configuration, index))
    } else {
      None
    }
  }

  def compare(rule: Rule): Int = {
    val compareFrom = from.compareTo(rule.from)
    if (compareFrom != 0) {
      compareFrom
    } else {
      to.compareTo(rule.to)
    }
  }
}

case class Unrestricted(f: Int, t: Int) extends Rule(f, t) {
  override def toString = stringFun(from, to, None)
  def toString(internalToName: Map[Int, String]) = {
    stringFun(internalToName, from, to, None)
  }
  def test(configuration: Vector[Int], index: Int): Boolean = testFromState(configuration, index)
}

abstract class QuantifierRule(f: Int, t: Int, st: Set[Int], si: Side, qs: String) extends Rule(f, t) {
  val quantifierString = qs
  val conditionStates = st
  val side = si

  def statesString(stateStrings: Set[String]): String = {
    val sideString = if (side == Left) "L" else "R"

    quantifierString + "." + sideString + "(" + stateStrings.toList.sorted.mkString(",") + ")"
  }

  def statesString(internalToName: Map[Int, String]): String = {
    statesString(conditionStates.map(internalToName(_)))
  }

  def statesString: String = statesString(conditionStates.map(_.toString))

  override def toString = stringFun(from, to, Some(statesString))

  def toString(internalToName: Map[Int, String]): String =
    stringFun(internalToName, from, to, Some(statesString(internalToName)))



  def statesForSide(configuration: Vector[Int], index: Int): SeqView[Int, Vector[Int]] = {
    side match {
      case Left => Configuration.leftOf(configuration, index)
      case Right => Configuration.rightOf(configuration, index)
    }
  }

  def testQuantifier(configuration: Vector[Int], index: Int): Boolean

  def test(configuration: Vector[Int], index: Int): Boolean = {
    testFromState(configuration, index) && testQuantifier(configuration, index)
  }
}

case class Existential(f: Int, t: Int, st: Set[Int], si: Side) extends QuantifierRule(f, t, st, si, "\u2203") {
  def   testQuantifier(configuration: Vector[Int], index: Int): Boolean = {
    val candidateStates = statesForSide(configuration, index)
    candidateStates.exists(st => conditionStates.contains(st))
  }
}
case class Universal(f: Int, t: Int, st: Set[Int], si: Side) extends QuantifierRule(f, t, st, si, "\u2200") {
  def testQuantifier(configuration: Vector[Int], index: Int): Boolean = {
    val candidateStates = statesForSide(configuration, index)
    candidateStates.forall(st => conditionStates.contains(st))
  }
}