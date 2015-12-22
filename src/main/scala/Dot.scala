object Dot {
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

  private def nodeName(configIndex: Int, stateIndex: Int): String = {
    "s_" + configIndex + "_" + stateIndex
  }

  private def indentationString(indentation: Int) = (0 until indentation).foldLeft("")((str, _) => str + "  ")

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

  private def makeConfiguration(configuration: Vector[Int],
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

  private def makeGraphStructure(content: String): String = {
    "digraph Configurations {\n" +
      content +
    "}\n"
  }

  private def makeIndexedConfigurations(indexedConfigurations: List[(Vector[Int], Int)],
                                    internalToName: Map[Int, String]): String = {

    indexedConfigurations.map{case(config, index) => makeConfiguration(config, index, internalToName)}.mkString
  }

  def makeConfigurations(configurations: Set[Vector[Int]], internalToName: Map[Int, String]): String = {
    val sortedAndIndexed = configurations.toList.sortWith(Configuration.compareBool).zipWithIndex
    makeGraphStructure(makeIndexedConfigurations(sortedAndIndexed, internalToName))
  }

  private def makeTransitionStates(states: Set[Int], internalToName: Map[Int, String]): String = {
    val stateNames = states.map(internalToName(_))
    val nameColourPair = stateNames.map(name => (colourForName(name), name))
    nameColourPair.map{
      case (Some(col), name) => "<font color=\"" + col + "\">\u25CF</font>"
      case (None, name) => name
    }.mkString(",")
  }

  private def makeRule(fromState: String,
                       toState: String,
                       rule: Rule,
                       internalToName: Map[Int, String],
                       indentation: Int): String = {

    val label = rule match {
      case Unrestricted(_, _) => ""
      case qr:QuantifierRule =>
        val stateString = makeTransitionStates(qr.conditionStates, internalToName)
        val sideString = if (qr.side == Left) "L" else "R"
        " [label=<" + qr.quantifierString + "<sub>" + sideString + "</sub>(" + stateString + ")>]"
    }

    val iString = indentationString(indentation)

    s"""$iString$fromState -> $toState$label\n"""
  }

  private def makeTransition(configFromIndex: Int,
                     configToIndex: Int,
                     stateIndex: Int,
                     rule: Rule,
                     internalToName: Map[Int, String]): String = {

    val fromState = nodeName(configFromIndex, stateIndex)
    val toState = nodeName(configToIndex, stateIndex)

    makeRule(fromState, toState, rule, internalToName, 2)
  }

  def makeConfigurationsWithTransitions(configsAndTransitions: Set[(Vector[Int], Int, Rule, Vector[Int])],
                                        internalToName: Map[Int, String],
                                        allTransitions: Boolean): String = {

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
  
  def makeViews(views: Set[Vector[Int]], internalToName: Map[Int, String], viewsPerRow: Int): String = {
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