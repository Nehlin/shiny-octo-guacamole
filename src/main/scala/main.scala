import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object main {
  def main(args:Array[String]): Unit = {
    val p = new Protocol("test_data/burns.pc")
    //val rulesGraph = Dot.makeRulesGraph(p.rules, p.internalToName)
    //Files.write(Paths.get("output/rules.dot"), rulesGraph.getBytes(StandardCharsets.UTF_8))

    //val c = Reachability.simple(Set(p.initialConfiguration.make(3)), p.rules)
    //val cDraw = Reachability.simpleWithTransitions(Set(p.initialConfiguration.make(3)), p.rules)
    //val graphData = Dot.makeConfigurationsWithTransitions(cDraw, p.internalToName, false)
    //val rulesData = Dot.makeRulesGraph(p.rules, p.internalToName)


    //val st2 = Reachability.simple(Set(p.initialConfiguration.make(3)), p.rules)
    //val configs = ViewsFromConfiguration.makeMultiple(st2, 2)
    //val viewsData = Dot.makeViews(configs, p.internalToName, 8)

    //Files.write(Paths.get("output/graph.dot"), graphData.getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("output/rules.dot"), rulesData.getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("output/views.dot"), viewsData.getBytes(StandardCharsets.UTF_8))

    val st2 = Set(
      Vector(1, 1),
      Vector(1, 2),
      Vector(2, 1)
    )

    val map = Concretisation.makeMap(st2)

    //val c3 = Concretisation.g(st2, map)
    //Files.write(Paths.get("output/v1.dot"), Dot.makeViews(c3, p.internalToName, 8).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("output/v3.dot"), Dot.makeViews(st2, p.internalToName, 8).getBytes(StandardCharsets.UTF_8))

  }
}

