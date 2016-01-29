import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object main {
  val OutputDir = "result/"

  def main(args:Array[String]): Unit = {
    val params = new Parameters(args)

    val protocolFile = params.get("ProtocolFile").asInstanceOf[String]
    val maxK = params.get("MaxK").asInstanceOf[Int]
    val makeProtocolDot = params.get("ProtocolDot").asInstanceOf[Boolean]
    val makeCounterDot = params.get("CounterDot").asInstanceOf[Boolean]
    val makeLogFile = params.get("LogFile").asInstanceOf[Boolean]

    val p = new Protocol(protocolFile)

    val existing = Set(Configuration.makeIdentifier(Vector(1, 3), p.alphabetLength))
    val x = Views.newViewsFromConfiguration(Vector(1, 2, 3), existing, p.alphabetLength)

    if (makeProtocolDot) {
      val protocolDotString = Dot.makeRulesGraph(p.rules, p.internalToName)
      writeToFile("protocol.dot", protocolDotString)
    }
  }

  def writeToFile(fileName: String, data: String): Unit = {
    val filePath = OutputDir + fileName
    Files.write(Paths.get(filePath), data.getBytes(StandardCharsets.UTF_8))
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
      //val newConfigs = AbstractPost.single(s, p.rules, map, 3)
      //Concretisation.addToRoMap(newConfigs, map)
      //if (newConfigs.subsetOf(s)) {
      //newFound = false
      //}
      //s = s ++ newConfigs
    }
    println(s.size)
  }
}

