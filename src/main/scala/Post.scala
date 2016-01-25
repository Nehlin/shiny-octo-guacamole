import scala.collection.mutable.{Set => MSet}

object Post {

  private def general[A](configuration: Vector[Int],
                         rules: Set[Rule],
                         ruleEvaluator: (Vector[Int], Rule, Int) => Option[A]): Set[A] = {

    val range = List.range(0, configuration.size)

    rules.flatMap(rule => {
      range.flatMap(index => {
        ruleEvaluator(configuration, rule, index)
      })
    })
  }

  def single(configuration: Vector[Int],
                     rules: Set[Rule]): Set[Vector[Int]] = {

    def ruleEvaluator(configuration: Vector[Int], rule: Rule, index: Int) = rule.testAndExecute(configuration, index)
    general(configuration, rules, ruleEvaluator)
  }


  def postWithTransitions(configuration: Vector[Int],
                                    rules: Set[Rule]): Set[(Vector[Int], Int, Rule, Vector[Int])] = {

    def ruleEvaluator(configuration: Vector[Int], rule: Rule, index: Int) = {
      rule.testAndExecute(configuration, index) match {
        case Some(config) => Some((configuration, index, rule, config))
        case None => None
      }
    }
    general(configuration, rules, ruleEvaluator)
  }

  /* Saves transitions. For printing purposes. */
  def iteratedWithTransitions(configurations: Set[Vector[Int]],
                              rules: Set[Rule]): Set[(Vector[Int], Int, Rule, Vector[Int])] = {

    var accumulatedConfigurations = configurations
    var newConfigurations = configurations
    var accumulatedCT = Set[(Vector[Int], Int, Rule, Vector[Int])]()
    var foundConfigurations = MSet[Vector[Int]]()

    while(newConfigurations.nonEmpty) {
      val currentCT = newConfigurations.flatMap(configuration => postWithTransitions(configuration, rules))
      val currentConfigurations = currentCT.map{case (_, _, _, to) => to}
      newConfigurations = currentConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations

      val added = MSet[Vector[Int]]()
      val newCT = currentCT.filter{case(_, _, _, to) =>
        // Wow! Such imperative!
        val res = newConfigurations.contains(to) && !added.contains(to)
        added += to
        res
      }
      foundConfigurations ++= newConfigurations
      accumulatedCT = accumulatedCT | newCT
    }
    accumulatedCT
  }

  def iterated(configurations: Set[Vector[Int]], rules: Set[Rule]): Set[Vector[Int]] = {
    var accumulatedConfigurations = configurations
    var newConfigurations = configurations

    while(newConfigurations.nonEmpty) {
      val resultingConfigurations = newConfigurations.flatMap(configuration => single(configuration, rules))
      newConfigurations = resultingConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations
    }
    accumulatedConfigurations
  }
}