package uu_tool_1

import scala.collection.mutable.{Set => MSet, ArrayBuffer}

object AbstractPost {

  def singleStepNaive(views: Set[ArrayBuffer[Int]],
                      rules: Set[Rule], k: Int): Set[ArrayBuffer[Int]] = {
    val concretisation = Concretisation.naive(views, k)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurationsFixed(newConfigs, None)
  }

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

  def fixPoint(views: Set[ArrayBuffer[Int]], rules: Set[Rule], k: Int) =
    fixPointNaive(views, rules, k)
}