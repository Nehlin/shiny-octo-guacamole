import scala.collection.mutable.ArrayBuffer

/**
 * A configuration is simply an ArrayBuffer of Ints, where each Int represents a state.
 * ArrayBuffers are used since they offer some of the advantages of an array
 * (mutability, constant access time), but are comparable. This is important since
 * arrays cannot easily be used in hash maps due to them not implementing equality
 * or hashCode properly.
 *
 * Arrays are by far the fastest data structure in Scala, when it comes to copying and
 * generating new instances. This comes at the price of non-functioning equality though so
 * ArrayBuffers is the next best thing. A combination might be feasible, but this could
 * also introduce extra overhead where conversions would be needed.
 */
object Configuration {

  /**
   * Returns the states to the left of an index
   * @param conf configuration
   * @param index index
   * @return the states that are to the left of index in configuration
   */
  def leftOf(conf: ArrayBuffer[Int], index: Int): ArrayBuffer[Int] = {
    conf.take(index)
  }

  /**
   * Returns the states to the right of an index
   * @param conf configuration
   * @param index index
   * @return the states that are to the right of index in configuration
   */
  def rightOf(conf: ArrayBuffer[Int], index: Int): ArrayBuffer[Int] = {
    conf.drop(index + 1)
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
  def compareBool(c1: ArrayBuffer[Int], c2: ArrayBuffer[Int]): Boolean = {
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
  def compare(c1: ArrayBuffer[Int], c2: ArrayBuffer[Int]): Int = {
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
