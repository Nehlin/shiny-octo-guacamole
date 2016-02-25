package uu_tool_1

import scala.collection.mutable.{Set => MSet, ArrayBuffer}

object AbstractPost {

  /**
   * Naive implementation of a single step of the abstract post.
   *
   * NOTE: This differs slightly from the non-naive in that it may also include the
   * views used to computed the new views, while the non-naive only computes new
   * views.
   *
   * @param views views to obtain concretisation from
   * @param rules set of transition rules
   * @param k length of the views
   * @return the set of views obtained by creating the concretisation of the
   *         views, applying the rules on the concretisation and creating views from
   *         the result.
   */
  def singleStepNaive(views: Set[ArrayBuffer[Int]],
                      rules: Set[Rule], k: Int): Set[ArrayBuffer[Int]] = {
    val concretisation = Concretisation.naive(views, k)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurationsFixed(newConfigs, None)
  }

  /**
   * Iterates the naive abstract post starting in a set of views until no new views are found
   *
   * @param views set of initial views
   * @param rules set of transition rules
   * @param k length of initial views
   * @return the set of views found from repeated application of naive abstract post.
   */
  def fixPointNaive(views: Set[ArrayBuffer[Int]], rules: Set[Rule], k: Int): Set[ArrayBuffer[Int]] = {
    var accumulatedViews = views
    var newViews = views

    while(newViews.nonEmpty) {
      val resultingViews = singleStepNaive(accumulatedViews, rules, k)
      newViews = resultingViews -- accumulatedViews
      accumulatedViews = accumulatedViews | newViews
    }
    accumulatedViews
  }

  /**
   * Generates the concretisation from a set of views, performs a post on these new
   * views, generates the views from this result.
   *
   * NOTE: the resulting views are only returned if the are not present in the original set
   *
   * @param views set of initial views
   * @param rules set of transition rules
   * @param preComputed set of configurations that are already explored. Any view constructed
   *                    by the concretisation that is already present in this set will be
   *                    ignored
   * @return the set of views obtained by creating the concretisation of the
   *         views, applying the rules on the concretisation and creating views from
   *         the result, where any new view already present in the initial views is
   *         ignored.
   */
  def singleStep(views: Set[ArrayBuffer[Int]],
                 rules: Set[Rule],
                 preComputed: Set[ArrayBuffer[Int]]): (Set[ArrayBuffer[Int]], Set[ArrayBuffer[Int]]) = {

    val concretisation = Concretisation.make(views, preComputed)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    (Views.fromConfigurationsFixed(newConfigs, Some(views)), concretisation)
  }

  /**
   * * Iterates the abstract post starting in a set of views until no new views are found
   *
   * @param views set of initial views
   * @param rules set of rules
   * @param preComputed set of previously explored views (see fixPointSingle)
   * @return the set of views found from repeated application of abstract post.
   */
  def fixPoint(views: Set[ArrayBuffer[Int]], rules: Set[Rule], preComputed: Set[ArrayBuffer[Int]]) = {
    var accumulatedViews = views
    var newViews = views
    var knownConcretisations = preComputed

    while(newViews.nonEmpty) {
      val (nv, fc) = singleStep(accumulatedViews, rules, knownConcretisations)
      newViews = nv
      knownConcretisations = knownConcretisations | fc
      accumulatedViews = accumulatedViews | newViews
    }
    accumulatedViews
  }
}