import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object main {
  def main(args:Array[String]): Unit = {
    val p = new Protocol("test_data/burns.txt")


    timeFunction(() => testBurnsNaive(p))
    timeFunction(() => testBurns(p))

    val m = Map(0 -> "Green", 1 -> "s1", 2 -> "YeLLoW", 3 -> "Panda")

    Files.write(Paths.get("output/example.dot"), Dot.makeConfigurations(Set(Vector(0, 1, 2, 3)), m).getBytes(StandardCharsets.UTF_8))
  }

  def timeFunction(f: () => Unit): Unit = {
    val t0 = System.nanoTime()
    f()
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + "ns")
  }

  def testBurnsNaive(p: Protocol) = {
    var s = Set(Vector(1, 1, 1))
    var newFound = true
    while (newFound) {
      val newConfigs = AbstractPost.singleNaive(s, p.rules, 3)
      if (newConfigs.subsetOf(s)) {
        newFound = false
      }
      s = s ++ newConfigs
    }
    println(s.size)
  }

  def testBurns(p: Protocol) = {
    var s = Set(Vector(1, 1, 1))
    val map = Concretisation.makeRoMap(s)
    var newFound = true
    while (newFound) {
      val newConfigs = AbstractPost.single(s, p.rules, map, 3)
      Concretisation.addToRoMap(newConfigs, map)
      if (newConfigs.subsetOf(s)) {
        newFound = false
      }
      s = s ++ newConfigs
    }
    println(s.size)
  }
}

