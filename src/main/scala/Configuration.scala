import scala.collection.SeqView

object Configuration {

  type Identifier = BigInt

  /**
   * Returns the states to the left of an index
   * @param conf configuration
   * @param index index
   * @return the states that are to the left of index in configuration
   */
  def leftOf(conf: Vector[Int], index: Int): SeqView[Int, Vector[Int]] = {
    conf.view.take(index)
  }

  /**
   * Returns the states to the right of an index
   * @param conf configuration
   * @param index index
   * @return the states that are to the right of index in configuration
   */
  def rightOf(conf: Vector[Int], index: Int): SeqView[Int, Vector[Int]] = {
    conf.view.drop(index + 1)
  }

  override def hashCode = toString.hashCode

  /**
   * Boolean version of compare. See compare for details.
   *
   * @param c1 first configuration
   * @param c2 second configuration
   * @return true iff c1 < c2
   */
  def compareBool(c1: Vector[Int], c2: Vector[Int]): Boolean = {
    compare(c1, c2) < 0
  }

  /**
   * Compares two configurations, for sorting. A longer list is always considered larger than a shorter one.
   * If the lists are of equal length, states are compared left to right until one configuration is found to have a
   * larger state than the other.
   *
   * @param c1 first configuration
   * @param c2 second configuration
   * @return 0 if c1 == c2, -1 if c1 < c2, 1 if c1 > c2
   */
  def compare(c1: Vector[Int], c2: Vector[Int]): Int = {
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

  // Offset is needed to ensure that (0) != (0, 0)
  def makeOffset(len: Int, aLen: Int): BigInt = {
    val bigALen:BigInt = aLen
    (bigALen * (bigALen.pow(len - 1) - 1)) / (bigALen - 1)
  }

  private def makeIdentifier_(conf: Vector[Int], aLen: Int, pos: Int, exp: BigInt, res: BigInt): (BigInt, BigInt) = {
    if (pos == -1) {
      (res, exp)
    } else {
      val curr = conf(pos) * exp
      makeIdentifier_(conf, aLen, pos - 1, exp*aLen, res + curr)
    }
  }

  /**
   * Creates a unique identifier for each configuration.
   *
   * NOTE: if this method is changed, make sure to update makeIdentifierSkip and makeIdentifierFixed as well or things
   * will break.
   *
   * @param conf configuration
   * @param aLen alphabet length
   * @return
   */
  def makeIdentifier(conf: Vector[Int], aLen: Int): Identifier = {
    val (value, _) = makeIdentifier_(conf, aLen, conf.length - 1, 1, 0)
    value + makeOffset(conf.length, aLen)
  }

  // TODO: Use vector view for better performance
  /**
   * The identifier of a configuration with two fixed states followed by a vector
   *
   * @param first first state in configuration
   * @param second second state in configuration
   * @param rest the remaining states of the configuration
   * @param aLen alphabet length
   * @return the identifier that would be created by calling makeIdentifier on the vector (first, second, rest)
   */
  def makeIdentifierFixedStart(first: Int, second: Int, rest: Vector[Int], aLen: Int): Identifier = {
    val (tailValue, exp) = makeIdentifier_(rest, aLen, rest.length - 1, 1, 0)
    val secondValue = second * exp
    val firstValue = first * exp * aLen
    val offset = makeOffset(rest.length + 2, aLen)
    firstValue + secondValue + tailValue + offset
  }

  /**
   * The identifier of a configuration after dropping a state
   *
   * @param conf original configuration
   * @param aLen length of alphabet
   * @param skipPos position to drop state
   * @return the identifier that would be created by calling makeIdentifier on conf after dropping the state at skipPos
   */
  def makeIdentifierSkip(conf: Vector[Int], aLen: Int, skipPos: Int): Identifier = {
    def makeIdentifierSkip_(pos: Int, exp: BigInt, res: BigInt): Identifier = {
      if (pos == -1) {
        res
      } else if (pos == skipPos) {
        makeIdentifierSkip_(pos - 1, exp, res)
      } else {
        val curr = conf(pos) * exp
        makeIdentifierSkip_(pos - 1, exp*aLen, res + curr)
      }
    }
    makeIdentifierSkip_(conf.length - 1, 1, 0) + makeOffset(conf.length - 1, aLen)
  }
}
