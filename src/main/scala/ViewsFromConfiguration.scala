import scala.collection.immutable.Stream.Empty

object ViewsFromConfiguration {
  def make[A](configuration: Vector[A], size: Int): Set[Vector[A]] = {

    def binarySum_(num: Int, sum: Int): Int = {
      if (num == 1) {
        1 + sum
      } else {
        val current = if (num % 2 == 1) 1 else 0
        binarySum_(num / 2, sum + current)
      }
    }

    /**
     * Returns the sum of all bits that are 1 in the binary representation of num.
     *
     * @param num positive integer. 0 or negative integers causes function to get stuck in infinite loop.
     * @example binarySum(37) = 3 // 37 = 100101
     * @example binarySum(32) = 1 // 37 = 100000
     */
    def binarySum(num: Int): Int = binarySum_(num, 0)

    def subWord_[A](vector: Vector[A], bin: Int, len: Int): Vector[A] = {
      val seq = for (
        i <- 0 until len
        if (1 << i & bin) != 0
      ) yield vector(i)
      seq.toVector
    }


    /**
     * Returns the sub-vector of vector that is specified by the binary representation of bin, in a right to left fashion
     *
     * While this function is not very intuitive, it is reasonably fast.
     *
     * @param vector original vector
     * @param bin integer whose binary representation selects which elements to select for sub-vector
     * @example subWord(Vector("A", "B", "C", "D", "E"), 1) = Vector("A") // 1 = 00001 => 10000
     * @example subWord(Vector("A", "B", "C", "D", "E"), 11) = Vector("A", "B", "D") //11 = 01011 => 11010
     */
    def subWord[A](vector: Vector[A], bin: Int): Vector[A] = {
      subWord_(vector, bin, vector.size)
    }

    val configLength = configuration.length

    val views = for (
      i <- 1 until 1 << configLength
      if binarySum(i) == size
    ) yield subWord(configuration, i)

    views.toSet
  }

  def makeMultiple[A](configurations: Set[Vector[A]], size: Int): Set[Vector[A]] = {

    def makeMultiple_[A](configurations: Set[Vector[A]], ack: Set[Vector[A]]): Set[Vector[A]] = {
      if (configurations.isEmpty) {
        ack
      } else {
        makeMultiple_(configurations.tail, ack ++ make(configurations.head, size))
      }
    }

    makeMultiple_(configurations, Set[Vector[A]]())
  }
}
