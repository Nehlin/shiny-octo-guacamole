package uu_tool_1

import scala.collection.mutable.ArrayBuffer

/**
 * A configuration is simply an ArrayBuffer of Ints, where each Int represents a state.
 * ArrayBuffers are used since they offer some of the advantages of an array
 * (mutability, constant access time), but are comparable. This is important since
 * arrays cannot easily be used in hash maps (and by extension, Scala Sets) due to them
 * not implementing equality or hashCode properly.
 *
 * Arrays are by far the fastest data structure in Scala, when it comes to copying and
 * generating new instances. This comes at the price of non-functioning equality though so
 * ArrayBuffers is the next best thing. See ConfigurationSet for more details.
 *
 * A combination might be feasible, but this could also introduce extra overhead where
 * conversions would be needed.
 *
 * NOTE: the comparison functions are kind of slow and should not be used other than for
 * debugging or generating .dot-files.
 *
 */
object Configuration {

  type Config = ArrayBuffer[Int]

  /**
   * Folds all states to the left of index with testFunction
   *
   * @param conf configuration
   * @param index index
   * @param initialVal initial value for accumulator in fold
   * @param testFunction function to run on each state. Will have access to the
   *                     accumulator
   * @tparam A type of result
   * @return the result of the folding
   */
  def leftOf[A](conf: Config,
                index: Int,
                initialVal: A,
                testFunction: (A, Int) => A): A = {

    (0 until index).foldLeft(initialVal)((res, i) => {
      testFunction(res, conf(i))
    })
  }

  /**
   * Folds all states to the right of index with testFunction
   *
   * @param conf configuration
   * @param index index
   * @param initialVal initial value for accumulator in fold
   * @param testFunction function to run on each state. Will have access to the
   *                     accumulator
   * @tparam A type of result
   * @return the result of the folding
   */
  def rightOf[A](conf: Config,
                index: Int,
                initialVal: A,
                testFunction: (A, Int) => A): A = {

    (index + 1 until conf.length).foldLeft(initialVal)((res, i) => {
      testFunction(res, conf(i))
    })
  }

  /**
   * Folds all states in a configuration except the one specified by index
   *
   * @param conf configuration
   * @param index index
   * @param initialVal initial value for accumulator in fold
   * @param testFunction function to run on each state. Will have access to the
   *                     accumulator
   * @tparam A type of result
   * @return the result of the folding
   */
  def bothSidesOf[A](conf: Config,
                     index: Int,
                     initialVal: A,
                     testFunction: (A, Int) => A): A = {

    (0 until conf.length).foldLeft(initialVal)((res, i) => {
      if (i == index) {
        res
      } else {
        testFunction(res, conf(i))
      }
    })
  }

  /**
   * Boolean version of compare. See compare for details.
   *
   * NOTE: kind of slow, should not be used for performance critical code
   *
   * @param c1 first configuration
   * @param c2 second configuration
   * @return true iff c1 < c2
   */
  def compareBool(c1: Config, c2: Config): Boolean = {
    compare(c1, c2) < 0
  }

  /**
   * Compares two configurations, for sorting. A longer list is always considered larger than a shorter one.
   * If the lists are of equal length, states are compared left to right until one configuration is found to have a
   * larger state than the other.
   *
   * NOTE: kind of slow, should not be used for performance critical code
   *
   * @param c1 first configuration
   * @param c2 second configuration
   * @return 0 if c1 == c2, -1 if c1 < c2, 1 if c1 > c2
   */
  def compare(c1: Config, c2: Config): Int = {
    def statesComparison(l1: List[Int], l2: List[Int]): Int = {
      (l1, l2) match {
        case (h1 :: t1, h2 :: t2) =>
          val res = h1.compareTo(h2)
          if (res == 0) statesComparison(t1, t2) else res
        case _ => 0
      }
    }

    val lengthComparison = c1.lengthCompare(c2.length)
    if (lengthComparison == 0) {
      statesComparison(c1.toList, c2.toList)
    } else {
      lengthComparison
    }
  }

}
