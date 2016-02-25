package uu_tool_1

import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet, ArrayBuffer}

object Concretisation {

  /**
   * Naive concretisation function. Note that this code needs to be replaced
   * for the final version. This is slow and can be rewritten with far better
   * performance. See this as placeholder code so that work can continue until
   * the real concretisation is implemented.
   *
   * The good thing about the naive method is that it is simple to understand
   * so it can be used for testing other implementations.
   *
   * @param views views to generate concretisation from
   * @param length length of views. length of concretisation is this length + 1
   * @return the concretisation of views.
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

  /**
   * A valid candidate of length n is a candidate where all subviews of length n-1 exist in views.
   *
   * Only views of length n-1 needs to be checked, since smaller views are subviews of these views.
   *
   * @param candidate potential new view of length n
   * @param views existing views of length n - 1
   * @return true iff all subviews of candidate of length n - 1 exist in views.
   */
  def testCandidate(candidate: ArrayBuffer[Int], views: Set[ArrayBuffer[Int]]): Boolean = {
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
      res = views.contains(currentView)

      i += 1
    }

    res
  }

  /**
   * A valid candidate is any view (a, b, c, d) where the view (a, b, c) and the
   * view (b, c, d) exist in views and the candidate is not already in preComputed
   *
   * @param views existing views to find candidates from.
   * @param preComputed set of already explored views.
   * @return set of all candidates that satisfies the requirements.
   */
  def findCandidates(views: Set[ArrayBuffer[Int]], preComputed: Set[ArrayBuffer[Int]]): Set[ArrayBuffer[Int]] = {
    /*
     * matchMap will contain all endings for a prefix. For the views (1, 2, 3),
     * (1, 2, 4), (5, 6, 7) it will be the map (1, 2) -> {3, 4}, (5, 6) -> {7}
     */
    val matchMap = views.map(view => {
      (view.take(view.length - 1), view.last)
    }).foldLeft(new MHashMap[ArrayBuffer[Int], MSet[Int]] with MMultiMap[ArrayBuffer[Int], Int]){
      case (acc, (arrKey, intVal)) => acc.addBinding(arrKey, intVal)
    }

    def newView(oldView: ArrayBuffer[Int], ending: Int): Option[ArrayBuffer[Int]] = {
      val nv = oldView.clone()
      nv.append(ending)
      if (preComputed.contains(nv)) {
        None
      } else{
        Some(nv)
      }
    }

    /*
     * matches the beginning of the views to the endings computed in matchMap
     */
    views.map(view => {
      matchMap.get(view.tail).map(endings => {
        endings.map(ending => newView(view, ending))
      })
    }).flatten.flatten.flatten
  }

  /**
   * Generates every view v of length n that satisfies the following:
   * - each subview of v of length n - 1 is present in views
   * - v is not present in preComputed
   *
   * @param views set of views to concretise
   * @param preComputed set of already explored views that should be ignored
   * @return the concretisation of views
   */
  def make(views: Set[ArrayBuffer[Int]], preComputed: Set[ArrayBuffer[Int]]): Set[ArrayBuffer[Int]] = {
    val candidates = findCandidates(views, preComputed)
    candidates.filter(testCandidate(_, views))
  }


}