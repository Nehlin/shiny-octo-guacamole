import java.beans.Expression

import sun.security.util.Length

import scala.util.matching.Regex

class InitialConfigurations(name: String, nameToInternal: Map[String, Int]) {
  val internal = nameToInternal(name)

  def make(length: Int): Vector[Int] = {
    Vector.fill(length)(internal)
  }
}