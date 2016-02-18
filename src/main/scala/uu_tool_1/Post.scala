package uu_tool_1

import scala.collection.mutable.{Set => MSet, ArrayBuffer}

/**
 * Functions for doing the normal post. This contains both the version that should be used
 * in the actual verification, and the much slower version that is used to create a .dot-file
 * of the transitions.
 */
object Post {

  /**
   * General method to go from one configuration to the set of configurations that is
   * directly reachable from there. This means testing and executing all rules from all
   * states in the configuration.
   *
   * @param configuration starting configuration
   * @param rules set of rules to create new configurations
   * @param ruleEvaluator function that evaluates a single rule
   * @tparam A type of return value. For simple posts, this should just be a configuration
   *           but for dot-friendly results, this is more complex
   * @return return value is dependent on ruleEvaluator, see single and singleWithTransitions
   *         for more details.
   */
  private def general[A](configuration: ArrayBuffer[Int],
                         rules: Set[Rule],
                         ruleEvaluator: (ArrayBuffer[Int], Rule, Int) => Option[A]): Set[A] = {

    val range = List.range(0, configuration.length)

    rules.flatMap(rule => {
      range.flatMap(index => {
        ruleEvaluator(configuration, rule, index)
      })
    })
  }

  /**
   * Executes all rules from all states in a single configuration and returns the resulting
   * configurations.
   *
   * @param configuration starting configuration
   * @param rules set of rules
   * @return all resulting configurations from executing rules from every state in configuration
   */
  def single(configuration: ArrayBuffer[Int],
                     rules: Set[Rule]): Set[ArrayBuffer[Int]] = {

    def ruleEvaluator(configuration: ArrayBuffer[Int], rule: Rule, index: Int) = rule.testAndExecute(configuration, index)
    general(configuration, rules, ruleEvaluator)
  }

  /**
   * Executes all rules from all states in a single configuration. For each successfully executed
   * rule, a tuple containing the starting configuration, the index of the state where the rule was
   * executed from, the rule and the resulting configuration is returned. The result of the function
   * is all such tuples
   *
   * @param configuration starting configuration
   * @param rules set of rules
   * @return a set of tuples containing starting configuration, state index where rule was executed,
   *         the rule used and the resulting configuration.
   */
  def singleWithTransitions(configuration: ArrayBuffer[Int],
                                  rules: Set[Rule]): Set[(ArrayBuffer[Int], Int, Rule, ArrayBuffer[Int])] = {

    def ruleEvaluator(configuration: ArrayBuffer[Int], rule: Rule, index: Int) = {
      rule.testAndExecute(configuration, index) match {
        case Some(config) => Some((configuration, index, rule, config))
        case None => None
      }
    }
    general(configuration, rules, ruleEvaluator)
  }

  /**
   * Computes post for a set of configurations, then computes repeated posts for the resulting
   * computations until no new configurations are found.
   *
   * @param configurations set of initial configurations
   * @param rules set of rules
   * @return the resulting set of configurations from running repeated posts from the
   *         initial configurations
   */
  def fixPoint(configurations: Set[ArrayBuffer[Int]], rules: Set[Rule]): Set[ArrayBuffer[Int]] = {
    var accumulatedConfigurations = configurations
    var newConfigurations = configurations

    while(newConfigurations.nonEmpty) {
      val resultingConfigurations = newConfigurations.flatMap(configuration => single(configuration, rules))
      newConfigurations = resultingConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations
    }
    accumulatedConfigurations
  }

  /**
   * Works as fixPoint, but save transitions like singleWithTransitions.
   *
   * Note that while a configuration may be reached multiple times, this function only
   * saves the rules for the first time it is found. It creates one counter example, not
   * all.
   *
   * @param configurations set of initial configurations
   * @param rules set of rules
   * @return a set of tuples containing starting configuration, state index where rule
   *         was executed, the rule used and the resulting configuration.
   */
  def fixPointWithTransitions(configurations: Set[ArrayBuffer[Int]],
                              rules: Set[Rule]): Set[(ArrayBuffer[Int], Int, Rule, ArrayBuffer[Int])] = {

    var accumulatedConfigurations = configurations
    var newConfigurations = configurations
    var accumulatedCT = Set[(ArrayBuffer[Int], Int, Rule, ArrayBuffer[Int])]()
    var foundConfigurations = MSet[ArrayBuffer[Int]]()

    while(newConfigurations.nonEmpty) {
      val currentCT = newConfigurations.flatMap(configuration => singleWithTransitions(configuration, rules))
      val currentConfigurations = currentCT.map{case (_, _, _, to) => to}
      newConfigurations = currentConfigurations -- accumulatedConfigurations
      accumulatedConfigurations = accumulatedConfigurations | newConfigurations

      val added = MSet[ArrayBuffer[Int]]()
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
}