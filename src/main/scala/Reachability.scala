import scala.collection.mutable.{Set => MSet}

object Reachability {

  def forwardsSingleGeneral[A](configuration: Configuration,
                         rules: Set[Rule],
                         ruleEvaluator: (Configuration, Rule, Int) => Option[A]): Set[A] = {

    val range = List.range(0, configuration.size)

    rules.flatMap(rule => {
      range.flatMap(index => {
        ruleEvaluator(configuration, rule, index)
      })
    })
  }

  def forwardsSingle(configuration: Configuration,
                     rules: Set[Rule]): Set[Configuration] = {

    def ruleEvaluator(configuration: Configuration, rule: Rule, index: Int) = rule.testAndExecute(configuration, index)
    forwardsSingleGeneral(configuration, rules, ruleEvaluator)
  }


  def forwardsSingleWithTransitions(configuration: Configuration,
                                    rules: Set[Rule]): Set[(Configuration, Int, Rule, Configuration)] = {

    def ruleEvaluator(configuration: Configuration, rule: Rule, index: Int) = {
      rule.testAndExecute(configuration, index) match {
        case Some(config) => Some((configuration, index, rule, config))
        case None => None
      }
    }
    forwardsSingleGeneral(configuration, rules, ruleEvaluator)
  }

  /* Saves transitions. For printing purposes. */
  def forwardsWithTransitions(configurations: Set[Configuration],
                              rules: Set[Rule]): Set[(Configuration, Int, Rule, Configuration)] = {

    var accumulatedConfigurations = configurations
    var newConfigurations = configurations
    var accumulatedCT = Set[(Configuration, Int, Rule, Configuration)]()
    var foundConfigurations = MSet[Configuration]()

    while(newConfigurations.nonEmpty) {
      val currentCT = newConfigurations.flatMap(configuration => forwardsSingleWithTransitions(configuration, rules))
      val currentConfigurations = currentCT.map{case (_, _, _, to) => to}
      newConfigurations = currentConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations

      val added = MSet[Configuration]()
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

  def forwards(configurations: Set[Configuration], rules: Set[Rule]): Set[Configuration] = {
    var accumulatedConfigurations = configurations
    var newConfigurations = configurations

    while(newConfigurations.nonEmpty) {
      val resultingConfigurations = newConfigurations.flatMap(configuration => forwardsSingle(configuration, rules))
      newConfigurations = resultingConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations
    }
    accumulatedConfigurations
  }
}