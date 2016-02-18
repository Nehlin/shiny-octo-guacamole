package uu_tool_1

import uu_tool_1.Configuration.Config

import scala.collection.mutable.{Set => MSet}

object AbstractPost {

  def singleStepNaive(views: Set[Config],
                      rules: Set[Rule], k: Int): Set[Config] = {
    val concretisation = Concretisation.naive(views, k)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurationsFixed(newConfigs, None)
  }

  def fixPointNaive(views: Set[Config], rules: Set[Rule], k: Int): Set[Config] = {
    var accumulatedViews = views
    var newViews = views

    while(newViews.nonEmpty) {
      val resultingViews = singleStepNaive(accumulatedViews, rules, k)
      newViews = resultingViews -- accumulatedViews
      accumulatedViews = accumulatedViews | newViews
    }
    accumulatedViews
  }

  def fixPoint(views: Set[Config], rules: Set[Rule], k: Int) =
    fixPointNaive(views, rules, k)
}