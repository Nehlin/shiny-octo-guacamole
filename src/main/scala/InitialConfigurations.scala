import java.beans.Expression

import sun.security.util.Length

import scala.util.matching.Regex

case class ParseException(message:String) extends Exception(message)
object InitialConfigurations {
  abstract class Token
  case object LParen extends Token
  case object RParen extends Token
  case object Repetition extends Token
  case object Or extends Token
  case class Variable(n: Int) extends Token {
    val name = n
  }

  abstract class ConfigurationExpression(vNames: Set[Int]) {
    val varNames = vNames
  }
  case class Constant(vNames: Set[Int]) extends ConfigurationExpression(vNames)
  case class Repeat(vNames: Set[Int]) extends ConfigurationExpression(vNames)

  def stringToTokenSeq(input: String, nameToInternal: Map[String, Int]): List[Token] = {
    if (input.isEmpty) {
      Nil
    } else {
      val tokenMatch: Option[Token] = input.head match {
        case '|' => Some(Or)
        case '(' => Some(LParen)
        case ')' => Some(RParen)
        case '*' => Some(Repetition)
        case _ => None
      }
      tokenMatch match {
        case Some(token) => List(token) ++ stringToTokenSeq(input.tail, nameToInternal)
        case None =>
          val re = "^[a-zA-Z]*".r
          val result = re.findFirstIn(input)
          result match {
            case Some(varName) =>
              val tailString = input.substring(varName.length)
              List(Variable(nameToInternal(varName))) ++ stringToTokenSeq(tailString, nameToInternal)
            case _ => throw new ParseException("invalid token found")
          }
      }
    }
  }

  def parseVariables(tokens: List[Token], vars: Set[Int]): (List[Token], Set[Int]) = {
    tokens match {
      case Variable(varName)::Or::tl => parseVariables(tl, vars + varName)
      case Variable(varName)::tl => (tl, vars + varName)
      case _ => throw new ParseException("cannot parse variables")
    }
  }

  def parseInternal(tokens: List[Token], result: List[ConfigurationExpression]): (List[Token], List[ConfigurationExpression]) = {
    if (tokens.isEmpty) {
      (Nil, result)
    } else if (tokens.head == LParen) {
      val (remainingTokens, varNames) = parseVariables(tokens.tail, Set[Int]())
      if (remainingTokens.head == RParen) {
        val (tail, newResult:ConfigurationExpression) = if (remainingTokens.tail.nonEmpty && remainingTokens.tail.head == Repetition) {
          (remainingTokens.tail.tail, Repeat(varNames))
        } else {
          (remainingTokens.tail, Constant(varNames))
        }
        parseInternal(tail, result ++ List(newResult))

      } else {
        throw new ParseException("cannot find right parenthesis")
      }
    } else {
      throw new ParseException("Invalid token found " + tokens.head)
    }
  }

  def parse(input: String, nameToInternal: Map[String, Int]): List[ConfigurationExpression] = {
    val tokens = stringToTokenSeq(input, nameToInternal)
    println(tokens)
    val (_, res) = parseInternal(tokens, List[ConfigurationExpression]())
    res
  }

  def makeTestRegex(expression: List[ConfigurationExpression]): String = {
    def x(y: Set[Int]): String = {
      "(" + y.mkString("|") + "),"
    }
    expression.map{
      case Repeat(vars) => "(" + x(vars) + ")*"
      case Constant(vars) => x(vars)
    }.mkString
  }

  def varsFromRules(rules: List[ConfigurationExpression]): Set[Int] = {
    rules.foldLeft(Set[Int]())((res, rule) =>
      res ++ rule.varNames
    )
  }

  // NOTE: Generates all possible configurations and filters them. Slow in some cases, but may not be the bottleneck.
  // Rewrite if needed
  def make(length: Int, vars: Set[Int], testRegex:String): Set[Configuration] = {
    val varVector = vars.toVector.sorted
    val numVars = varVector.length
    def makeList(i: Int, len: Int): List[Int] = {
      def ml(i: Int, len: Int): List[Int] = {
        if (len == 0) {
          Nil
        } else {
          varVector(i % numVars) :: ml(i / numVars, len - 1)
        }
      }
      ml(i, len).reverse
    }
    def test(i: Int): Boolean = {
      val str = makeList(i, length).mkString(",") + ","
      str.matches(testRegex)
    }
    val configs = for {
      i <- 0.until(math.pow(numVars, length).toInt)
      if test(i)
    } yield {
      new Configuration(makeList(i, length))
    }

    configs.toSet
  }
}