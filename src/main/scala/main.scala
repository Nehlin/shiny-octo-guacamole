import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object main {
  def main(args:Array[String]): Unit = {
    val p = new Protocol("test_data/burns.pc")
    p.makeInitialConfigurations(7).toList.sorted.foreach(println(_))



    //val cInit = Set(new Configuration(Vector(0)), new Configuration(Vector(0, 0)), new Configuration(Vector(0, 0, 0)))
    //val cInit = Set(new Configuration(Vector(0)), new Configuration(Vector(0, 0)))
    //val rules: Set[Rule] = Set(Unrestricted(0, 1), Unrestricted(1, 2), Unrestricted(0, 1), Existential(0, 3, Set(2), Left), Existential(0, 4, Set(0, 4), Right))
    //val c = Reachability.forwards(cInit, rules)
    //val graphData = Dot.makeConfigurationsWithTransitions(Reachability.forwardsWithTransitions(cInit, p.rules), p.internalToName, false)
    //Files.write(Paths.get("output/graph.dot"), graphData.getBytes(StandardCharsets.UTF_8))
    //InitialConfigurations.parse("(Apa|Ko)*(Get)")
    //println(InitialConfigurations.parse("(hej|hopp)*(katt)"))
  }
}

