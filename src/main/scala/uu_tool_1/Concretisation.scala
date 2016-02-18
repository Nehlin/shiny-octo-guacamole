package uu_tool_1

import uu_tool_1.Configuration.Config

import scala.collection.mutable.{HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet, ArrayBuffer}

object Concretisation {

  /* Naive concretisation function. Note that this code needs to be replaced
   * for the final version. This is slow and can be rewritten with far better
   * performance. See this as placeholder code so that work can continue until
   * the real concretisation is implemented.
   *
   * The good thing about the naive method is that it is simple to understand
   * so it can be used for testing other implementations.
   */
  def naive(views: Set[Config], length: Int): Set[Config] = {
    val alphabet = views.foldLeft(Set[Int]())((acc, curr) => acc ++ curr.toSet).toArray
    val aLen = alphabet.length

    val mSet = MSet[Config]()

    def configForNum(num: Int, pos: Int): Config = {
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

  def testCandidate(candidate: Config, views: MSet[Config]): Boolean = {
    var excludedState = candidate.head
    var excludedSwap = 0
    val currentView = candidate.tail

    var res = true
    var i = -1
    while (i < currentView.length && res) {
      if (i >= 0) {
        excludedSwap = currentView(i)
        currentView(i) = excludedState
        excludedState = excludedSwap
      }
      println(currentView, views.contains(currentView))
      res = views.contains(currentView)

      i += 1
    }

    res
  }

  def findCandidates(views: Set[Config]): Set[Config] = {
    println(views)
    Set()
  }
/*
  type RoMap = MHashMap[Int, MSet[Config]] with MMultiMap[Int, Config]

  def addToRoMap(views: Set[Config], roMap: RoMap): Unit = {
    views.map(view => {
      val key = view.head
      roMap.addBinding(key, view.drop(1))
    })
  }

  def makeRoMap(views: Set[Config]): RoMap = {
    val roMap = new MHashMap[Int, MSet[Config]] with MMultiMap[Int, Config]

    addToRoMap(views, roMap)

    roMap
  }

  def generate(views: Set[Config],
               roMap: RoMap,
               existing: MSet[Config]): Set[Config] = {

    // All views should be of equal length, so this is valid.
    val viewLength = views.head.length

    // Create once and reuse
    val currentCandidate = ArrayBuffer.fill(viewLength + 1){0}

    def generateCandidates(view: Config): Option[MSet[Config]] = {

      val firstState = view.head
      val secondState = view.tail.head

      def updateCurrentCandidate(first: Int, second: Int, rest: Config): Config = {
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

    def testCandidate(candidate: Config, existing: MSet[Config]): Boolean = {

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