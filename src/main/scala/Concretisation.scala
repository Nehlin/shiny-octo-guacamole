import collection.mutable.{ HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet }
import scala.collection.SeqView

object Concretisation {

  type SubView = SeqView[Int, Vector[Int]]
  type RoMap = MHashMap[Int, MSet[Vector[Int]]] with MMultiMap[Int, Vector[Int]]


  // TODO: use vector views instead of .tail
  def addToRoMap(views: Set[Vector[Int]], roMap: RoMap): Unit = {
    views.map(view => {
      val key = view.head
      roMap.addBinding(key, view.drop(1))
    })
  }

  def makeRoMap(views: Set[Vector[Int]]): RoMap = {
    val roMap = new MHashMap[Int, MSet[Vector[Int]]] with MMultiMap[Int, Vector[Int]]

    addToRoMap(views, roMap)

    roMap
  }

  // Naive concretisation function. For testing purposes only!
  def naive(views: Set[Vector[Int]], length: Int): Set[Vector[Int]] = {
    val alphabet = views.foldLeft(Set[Int]())((acc, curr) => acc ++ curr.toSet).toVector
    val aLen = alphabet.length

    val mSet = MSet[Vector[Int]]()

    def configForNum(num: Int, pos: Int): Vector[Int] = {
       if (pos == 0) {
         Vector(alphabet(num))
       } else {
         configForNum(num / aLen, pos - 1) ++ Vector(alphabet((num % aLen).toInt))
       }
    }

    (0 until math.pow(aLen.toDouble, (length + 1).toDouble).toInt).foreach(num => {
      val candidate = configForNum(num, length)
      val cViews = Views.fromConfiguration(candidate, length)
      if (cViews.subsetOf(views)) {
        mSet += candidate
      }
    })
    mSet.toSet
  }

  def generate(views: Set[Vector[Int]],
               roMap: RoMap,
               aLen: Int,
               existing: MSet[Configuration.Identifier]): Set[Vector[Int]] = {

    val viewIdentifiers = views.map(v => Configuration.makeIdentifier(v, aLen))

    def generateCandidates(view: Vector[Int]): Option[MSet[Vector[Int]]] = {

      val secondState = view.tail.head

      if (roMap.isDefinedAt(secondState)) {
        val endings = roMap(secondState)
        val candidates = for (ending <- endings
          if existing.contains(Configuration.makeIdentifierFixedStart(view.head, secondState, ending, aLen))
        ) yield {
          view.head +: secondState +: ending
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

    def testCandidate(candidate: Vector[Int], existing: MSet[Configuration.Identifier]): Boolean = {

      def candidateIdentifier = Configuration.makeIdentifier(candidate, aLen)

      def testCandidate_(pos: Int): Boolean = {
        if (pos == candidate.length) {
          existing.add(candidateIdentifier)
          true
        } else {
          val subVecIdentifier = Configuration.makeIdentifierSkip(candidate, aLen, pos)
          if (viewIdentifiers.contains(subVecIdentifier)) {
            testCandidate_(pos + 1)
          } else {
            false
          }
        }
      }

      if (existing.contains(candidateIdentifier)) {
        false
      } else {
        testCandidate_(0)
      }
    }

    val candidates = views.map(generateCandidates).flatten.flatten
    candidates.filter(candidate => testCandidate(candidate, existing))
  }
}