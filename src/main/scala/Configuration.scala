import scala.collection.SeqView

class Configuration(s:Vector[Int]) extends Ordered[Configuration] {
  val states = s

  def this(s: List[Int]) = {
    this(s.toVector)
  }

  def size: Int = states.length

  def aAtIndex(index: Int): Int = {
    states(index)
  }

  def leftOf(index: Int): SeqView[Int, Vector[Int]] = {
    states.view.take(index)
  }

  def rightOf(index: Int): SeqView[Int, Vector[Int]] = {
    states.view.drop(index + 1)
  }

  def updated(index: Int, newState: Int): Configuration = {
    new Configuration(states.updated(index, newState))
  }

  override def toString = "Configuration(" + states.mkString(", ") + ")"

  override def equals(o: Any) = o match {
    case c: Configuration => c.states == states
    case _ => false
  }

  override def hashCode = toString.hashCode

  // NOTE: this is an inefficient comparison that should only be used for testing/debugging
  def compare(configuration: Configuration): Int = {
    def statesComparison(l1: List[Int], l2: List[Int]): Int = {
      (l1, l2) match {
        case (h1::t1, h2::t2) =>
          val res = h1.compareTo(h2)
          if (res == 0) statesComparison(t1, t2) else res
        case _ => 0 // Should not occur since we already ensured lists are of equal length
      }
    }

    val lengthComparison = states.lengthCompare(configuration.states.length)
    if (lengthComparison == 0) {
      statesComparison(states.toList, configuration.states.toList)
    } else {
      lengthComparison
    }
  }
}
