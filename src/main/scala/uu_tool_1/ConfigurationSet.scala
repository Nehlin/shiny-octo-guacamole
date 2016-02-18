package uu_tool_1

/**
 * One of the big bottlenecks of Scala is the slowness of scala data structures
 * compared to raw Java arrays. Scala ArrayBuffers allow insertion into sets
 * which Arrays does not, and this is crucial in many parts of the program.
 *
 * This file contains classes that implement sets holding configurations as Java-
 * arrays. Currently they support creation and insertion. To use this as intended,
 * extend to handle lookup, set union and difference.
 *
 * Implementing this completely is a good first step if going from ArrayBuffers
 * to Arrays is desirable.
 */

object ConfigurationSet {

  private def insert_(configuration: Array[Int],
                      alphabetSize: Int,
                      configSet: Array[Any],
                      index: Int): Unit = {

    if (index < configuration.length) {
      val currentState = configuration(index)

      if (configSet(currentState) == null) {
        configSet(currentState) = new Array(alphabetSize)
      }
      insert_(configuration, alphabetSize, configSet(currentState).asInstanceOf[Array[Any]], index + 1)
    }
  }

  def insert(configuration: Array[Int], alphabetSize: Int, configSet: Array[Any]) = {
    insert_(configuration, alphabetSize, configSet, 0)
  }

  def mkString(configSet: Any) = {
    if (configSet == null) {
      "."
    } else {
      val arr = configSet.asInstanceOf[Array[Any]]
      "[" + (0 until arr.length).map(index =>  if (arr(index) == null) "." else "O").mkString("][") + "]"
    }
  }

  def makeNew(alphabetSize: Int): Array[Any] = {
    new Array(alphabetSize)
  }
}