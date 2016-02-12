import scala.collection.mutable.{HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet, ArrayBuffer}
import scala.collection.SeqView

object Concretisation {

  /* Naive concretisation function. Note that this code needs to be replaced
   * for the final version. This is extremely slow and can be rewritten with
   * far better performance. See this as placeholder code so that work can
   * continue until the real concretisation is implemented.
   */
  def naive(views: Set[ArrayBuffer[Int]], length: Int): Set[ArrayBuffer[Int]] = {
    val alphabet = views.foldLeft(Set[Int]())((acc, curr) => acc ++ curr.toSet).toArray
    val aLen = alphabet.length

    val mSet = MSet[ArrayBuffer[Int]]()

    def configForNum(num: Int, pos: Int): ArrayBuffer[Int] = {
      if (pos == 0) {
        ArrayBuffer(alphabet(num))
      } else {
        configForNum(num / aLen, pos - 1) ++ ArrayBuffer(alphabet(num % aLen))
      }
    }

    (0 until math.pow(aLen.toDouble, (length + 1).toDouble).toInt).foreach(num => {
      val candidate = configForNum(num, length)
      val cViews = Views.fromConfigurationFixed(candidate, None)
      if (cViews.subsetOf(views)) {
        mSet += candidate
      }
    })
    mSet.toSet
  }
/*
  type RoMap = MHashMap[Int, MSet[ArrayBuffer[Int]]] with MMultiMap[Int, ArrayBuffer[Int]]

  def addToRoMap(views: Set[ArrayBuffer[Int]], roMap: RoMap): Unit = {
    views.map(view => {
      val key = view.head
      roMap.addBinding(key, view.drop(1))
    })
  }

  def makeRoMap(views: Set[ArrayBuffer[Int]]): RoMap = {
    val roMap = new MHashMap[Int, MSet[ArrayBuffer[Int]]] with MMultiMap[Int, ArrayBuffer[Int]]

    addToRoMap(views, roMap)

    roMap
  }

  def generate(views: Set[ArrayBuffer[Int]],
               roMap: RoMap,
               existing: MSet[ArrayBuffer[Int]]): Set[ArrayBuffer[Int]] = {

    // All views should be of equal length, so this is valid.
    val viewLength = views.head.length

    // Create once and reuse
    val currentCandidate = ArrayBuffer.fill(viewLength + 1){0}

    def generateCandidates(view: ArrayBuffer[Int]): Option[MSet[ArrayBuffer[Int]]] = {

      val firstState = view.head
      val secondState = view.tail.head

      def updateCurrentCandidate(first: Int, second: Int, rest: ArrayBuffer[Int]): ArrayBuffer[Int] = {
        currentCandidate(0) = first
        currentCandidate(1) = second
        (0 until rest.length).foreach(index => currentCandidate(index + 2) = rest(index))
        currentCandidate
      }

      if (roMap.isDefinedAt(secondState)) {
        val endings = roMap(secondState)
        val candidates = for (ending <- endings
          if !existing.contains(updateCurrentCandidate(firstState, secondState, ending))
        ) yield {
          currentCandidate.clone()
        }
        if (candidates.nonEmpty) {
          Some(candidates)
        } else {
          None
        }
      } else {
        None
      }
    }

    val testView = ArrayBuffer.fill(viewLength)(0)
    var excludedState = 0

    def testCandidate(candidate: ArrayBuffer[Int], existing: MSet[ArrayBuffer[Int]]): Boolean = {

      def testCandidate_(pos: Int): Boolean = {
        if (pos == candidate.length) {
          true
        } else {
          if (pos > 0) {
            val newExcluded = testView(pos - 1)
            testView(pos - 1) = excludedState
            excludedState = newExcluded
          }
          if (views.contains(testView)) {
            testCandidate_(pos + 1)
          } else {
            false
          }
        }
      }

      excludedState = candidate.head
      for (i <- 0 until testView.length) {
        testView(i) = candidate(i + 1)
      }
      testCandidate_(0)
    }

    val candidates = views.map(generateCandidates).flatten.flatten
    candidates.filter(candidate => testCandidate(candidate, existing))
  }*/
}