import scala.collection.SeqView

object Configuration {
  def leftOf(configuration: Vector[Int], index: Int): SeqView[Int, Vector[Int]] = {
    configuration.view.take(index)
  }

  def rightOf(configuration: Vector[Int], index: Int): SeqView[Int, Vector[Int]] = {
    configuration.view.drop(index + 1)
  }

  override def hashCode = toString.hashCode

  def compareBool(c1: Vector[Int], c2: Vector[Int]): Boolean = {
    compare(c1, c2) < 0
  }

  def compare(c1: Vector[Int], c2: Vector[Int]): Int = {
    def statesComparison(l1: List[Int], l2: List[Int]): Int = {
      (l1, l2) match {
        case (h1 :: t1, h2 :: t2) =>
          val res = h1.compareTo(h2)
          if (res == 0) statesComparison(t1, t2) else res
        case _ => 0 // Should not occur since we already ensured lists are of equal length
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
