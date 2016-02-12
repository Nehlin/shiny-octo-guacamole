import scala.collection.mutable.ArrayBuffer

/**
 * Dot is a format for displaying graphs. It is a human readable format.
 * More information about the format can be found here:
 * http://www.graphviz.org/content/dot-language
 * NOTE: This code is not covered by any test-cases as it is both non-
 * essential and hard to test.
 */
object Dot {
  /**
   * Some state names will create coloured states instead of named states. To
   * support additional colours, add the state name to the left of the switch
   * and Some("colour") to the right, where "colour" is a colour supported by
   * the .dot-format.
   *
   * @param name name of state that should be treated as a colour. case
   *             insensitive
   * @return Some(colourString) if name is a supported colour, None otherwise
   */
  private def colourForName(name: String): Option[String] = {
    name.toLowerCase match {
      case "red" => Some("red")
      case "green" => Some("yellowgreen")
      case "blue" => Some("blue")
      case "black" => Some("black")
      case "white" => Some("white")
      case "yellow" => Some("yellow")
      case _ => None
    }
  }

  /**
   * Internal unique name of a state. This has to be unique, but will not be
   * visible in the graphical representation.
   *
   * @param configIndex combined with stateIndex, this has to be unique
   * @param stateIndex combined with configIndex, this has to be unique
   * @return a unique name for the node
   */
  private def nodeName(configIndex: Int, stateIndex: Int): String = {
    "s_" + configIndex + "_" + stateIndex
  }

  /**
   * Creates indentation to create prettier .dot-files. Looks nice, but not really needed.
   * @param indentation level of indentation
   * @return a string containing spaces for indentation.
   */
  private def indentationString(indentation: Int) = (0 until indentation).foldLeft("")((str, _) => str + "  ")

  /**
   * Creates a .dot-string for a single state.
   * @param name will be used as the label for the state if the state is not
   *             coloured, ignored if it is
   * @param configIndex combined with stateIndex, this has to be unique
   * @param stateIndex combined with configIndex, this has to be unique
   * @param colour optional colour. See: colourForName
   * @param indentation level of indentation
   * @return a string representing a single state
   */
  private def singleState(name: String,
                          configIndex: Int,
                          stateIndex: Int,
                          colour: Option[String],
                          indentation: Int): String = {

    val (colourString, labelString) = colour match {
      case Some(col) => (s",fillcolor=$col", "")
      case None => ("", name)
    }

    val iString = indentationString(indentation)

    val internalName = nodeName(configIndex, stateIndex)
    s"""$iString$internalName [shape=circle,style=filled,fixedsize=true,width=0.5,label="$labelString"$colourString]\n"""
  }

  /**
   * Creates a graph representing the transition rules of a protocol.
   *
   * @param rules set of rules to create a graph from
   * @param internalToName conversion from internal state representation to original name
   * @return a string containing the complete graph representation of a set of rules.
   */
  def makeRulesGraph(rules: Set[Rule], internalToName: Map[Int, String]): String = {
    val states = rules.foldLeft(Set[Int]())((set, rule) => {
      set + rule.from + rule.to
    }).toList.sorted

    val stateIndexMap = states.zipWithIndex.map{case (state, index) => (state, index)}.toMap

    val stateStrings = states.map(state => {
      val name = internalToName(state)
      val colour = colourForName(name)
      val index = stateIndexMap(state)
      singleState(internalToName(state), 0, index, colour, 1)
    }).mkString

    val ruleStrings = rules.toList.sorted.map(rule => {
      val fromState = nodeName(0, stateIndexMap(rule.from))
      val toState = nodeName(0, stateIndexMap(rule.to))
      makeRule(fromState, toState, rule, internalToName, 1)
    }).mkString

    makeGraphStructure(stateStrings + ruleStrings)
  }

  /**
   * Creates a .dot-string for a single configuration
   *
   * @param configuration input configuration
   * @param configIndex index of configuration. This should be unique
   * @param internalToName conversion from internal state representation to original name
   * @return the .dot-string that represents configuration
   */
  private def makeConfiguration(configuration: ArrayBuffer[Int],
                                configIndex: Int,
                                internalToName: Map[Int, String]): String = {

    val states = configuration
    val stateNames = states.map(internalToName(_))
    val statePrintData = stateNames.zipWithIndex.map{case (name, index) => (name, index, colourForName(name))}
    val nodeNames = List.range(0, states.length).map(nodeName(configIndex, _))

    val statesString = statePrintData.map{case (stateName, stateIndex, colour) =>
      singleState(stateName, configIndex, stateIndex, colour, 2)
    }.mkString

    val rankString = if (states.length > 1) {
      "    {rank=same; " + nodeNames.mkString(" ") + "}\n" +
      "    " + nodeNames.mkString(" -> ") + " [style=invis]\n"

    } else {
      ""
    }

    "  subgraph cluster_" + configIndex + "{\n" +
      statesString +
      rankString +
      "  }\n"
  }

  /**
   * Wraps content inside a graph element. This is typically the last thing
   * done once all graph content is created.
   *
   * @param content content of graph
   * @return content, wrapped inside a .dot-graph structure.
   */
  private def makeGraphStructure(content: String): String = {
    "digraph Configurations {\n" +
      content +
    "}\n"
  }

  /**
   * Takes a list of (configuration, index)-pairs and generates the
   * .dot-strings for each configuration, using the matching index as
   * configuration index.
   *
   * @param indexedConfigurations list of (configuration, index)-pairs.
   * @param internalToName conversion from internal state representation to original name
   * @return .dot string with the graph-data of indexedConfigurations
   */
  private def makeIndexedConfigurations(indexedConfigurations: List[(ArrayBuffer[Int], Int)],
                                    internalToName: Map[Int, String]): String = {

    indexedConfigurations.map{case(config, index) => makeConfiguration(config, index, internalToName)}.mkString
  }

  /**
   * Takes a set of configurations and generates the .dot-string for the graph
   * containing all of them
   * @param configurations configurations for the graph
   * @param internalToName conversion from internal state representation to original name
   * @return .dot string with the graph-data of configurations
   */
  def makeConfigurations(configurations: Set[ArrayBuffer[Int]], internalToName: Map[Int, String]): String = {
    val sortedAndIndexed = configurations.toList.sortWith(Configuration.compareBool).zipWithIndex
    makeGraphStructure(makeIndexedConfigurations(sortedAndIndexed, internalToName))
  }

  /**
   * Creates the states on a label of a transition. If any reserved colour
   * names are used as state names, a circle of that colour will be used
   * instead of state name.
   *
   * @param states set of states that are the condition for the transition
   * @param internalToName conversion from internal state representation to original name
   * @return string for label text containing states
   */
  private def makeTransitionStates(states: Set[Int], internalToName: Map[Int, String]): String = {
    val stateNames = states.map(internalToName(_))
    val nameColourPair = stateNames.map(name => (colourForName(name), name))
    nameColourPair.map{
      case (Some(col), name) => "<font color=\"" + col + "\">\u25CF</font>"
      case (None, name) => name
    }.mkString(",")
  }

  /**
   * Creates a rule-transition between two states
   *
   * @param fromState source state of the transition
   * @param toState target state of the transition
   * @param rule rule of the transition
   * @param internalToName conversion from internal state representation to original name
   * @param indentation level of indentation
   * @return .dot-string that represents the transition
   */
  private def makeRule(fromState: String,
                       toState: String,
                       rule: Rule,
                       internalToName: Map[Int, String],
                       indentation: Int): String = {

    val label = rule match {
      case Unrestricted(_, _) => ""
      case qr:QuantifierRule =>
        val stateString = makeTransitionStates(qr.conditionStates, internalToName)
        val sideString = if (qr.side == Left) "<sub>L</sub>" else if (qr.side == Right) "<sub>R</sub>" else ""
        " [label=<" + qr.quantifierString + sideString + "(" + stateString + ")>]"
    }

    val iString = indentationString(indentation)

    s"""$iString$fromState -> $toState$label\n"""
  }

  /**
   * Creates a transition between the same state in two configurations
   *
   * @param configFromIndex index of source configuration
   * @param configToIndex index of target configutraion
   * @param stateIndex index of state (in both configurations)
   * @param rule rule of the transition
   * @param internalToName conversion from internal state representation to original name
   * @return .dot-string that represents the transition
   */
  private def makeTransition(configFromIndex: Int,
                     configToIndex: Int,
                     stateIndex: Int,
                     rule: Rule,
                     internalToName: Map[Int, String]): String = {

    val fromState = nodeName(configFromIndex, stateIndex)
    val toState = nodeName(configToIndex, stateIndex)

    makeRule(fromState, toState, rule, internalToName, 2)
  }

  /**
   * Creates a graph representing a series of posts. See Post.iteratedWithTransitions
   *
   * @param configsAndTransitions Each entry in this set represents a transition from state
   *                              index in the the first configuration, to state index in
   *                              the second configuration via the rule.
   * @param internalToName conversion from internal state representation to original name
   * @return .dot-graph representing the series of posts represented by
   *         configsAndTransitions
   */
  def makeConfigurationsWithTransitions(configsAndTransitions: Set[(ArrayBuffer[Int], Int, Rule, ArrayBuffer[Int])],
                                        internalToName: Map[Int, String]): String = {

    val lhs = configsAndTransitions.map{case(c, _, _, _) => c}
    val rhs = configsAndTransitions.map{case(_, _, _, c) => c}
    val allConfigurations = lhs | rhs
    val indexedConfigurations = allConfigurations.toList.sortWith(Configuration.compareBool).zipWithIndex
    val indexMap = indexedConfigurations.toMap

    val configString = makeIndexedConfigurations(indexedConfigurations, internalToName)

    val transitionString = configsAndTransitions.map{case(from, stateIndex, rule, to) =>
      val configFromIndex = indexMap(from)
      val configToIndex = indexMap(to)
      makeTransition(configFromIndex, configToIndex, stateIndex, rule, internalToName)
    }.mkString

    makeGraphStructure(configString + transitionString)
  }

  /**
   * Creates a graph representing a set of views, positioned in a grid
   *
   * @param views set of views (configurations) to display
   * @param internalToName conversion from internal state representation to original name
   * @param viewsPerRow number of views per row
   * @return a .dot-graph containing views positioned in a grid.
   */
  def makeViews(views: Set[ArrayBuffer[Int]], internalToName: Map[Int, String], viewsPerRow: Int): String = {
    val sortedViews = views.toList.sortWith(Configuration.compareBool)

    val configs = makeIndexedConfigurations(sortedViews.zipWithIndex, internalToName)
    val rowTransitions = (0 until sortedViews.length - viewsPerRow).map(index => {
      val fromState = nodeName(index, 0)
      val toState = nodeName(index + viewsPerRow, 0)
      indentationString(1) + fromState + " -> " + toState + " [style=invis]\n"
    }).mkString

    makeGraphStructure(configs + rowTransitions)
  }
}