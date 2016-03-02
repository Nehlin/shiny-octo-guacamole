import uu_tool_1._

import scala.collection.mutable.ArrayBuffer

object main {

  def main(args:Array[String]): Unit = {
    val parameters = new Parameters(args)

    val protocolFile = parameters.getProtocolFile
    val makeProtocolDot = parameters.getMakeProtocolDot
    val makeCounterDot = parameters.getMakeCounterDot
    val makeLogFile = parameters.getMakeLogFile
    val maxK = parameters.getMaxK

    println("Parsing protocol file")
    val protocol = new Protocol(protocolFile)

    if (makeProtocolDot == Only || makeProtocolDot == Yes) {
      println("Generating protocol file in: " + FileWriter.ProtocolDotFile)
      val protocolDotString = Dot.makeRulesGraph(protocol.rules, protocol.internalToName)
      FileWriter.write(protocolDotString, FileWriter.ProtocolDotFile)
      if (makeProtocolDot == Only) {
        println("ProtocolDot was set to \"only\", exiting")
        return
      }
    }

    println("Verifying protocol up to maximum size of " + maxK)
    val (result, endK) = Verify.run(protocol, maxK)
    println("Result: " + Verify.resultToString(result))

    // If the protocol is unsafe and the user requested a counter example,
    // generate one.
    if (result == Unsafe && makeCounterDot) {
      println("Generating counter example file: " + FileWriter.CounterDotFile)
      val initialConfiguration = Set(protocol.initialConfiguration(endK))
      val postConfigs = Post.fixPointWithTransitions(initialConfiguration, protocol.rules)
      val dotString = Dot.makeConfigurationsWithTransitions(postConfigs, protocol.internalToName)
      FileWriter.write(dotString, FileWriter.CounterDotFile)
    }

    if (makeLogFile) {
      // TODO: update this to contain more useful information
      // Information should be gathered from Verify.run and could contain information such as
      // number of created configurations and execution time.

      println("Writing log to file: " + FileWriter.LogFile)
      val logString =
        "Result:\n" +
        Verify.resultToString(result) + "\n" +
        (if (result == Unclear) {
          "Unable to reach a result for configurations of size up to: " + endK + "\n"
        } else {
          "Result found at k = " + endK + "\n"
        })

      FileWriter.write(logString, FileWriter.LogFile)
    }
  }


  /**
   * Times and prints the execution of f. Not very reliable when comparing two
   * functions to figure out which is faster. To get a usable result when comparing
   * two functions (f1, f2), make sure you do something like this:
   * timeFunction(f1)
   * timeFunction(f2)
   * timeFunction(f2)
   * timeFunction(f1)
   *
   * This usually gives a decent idea of which task is faster, at least if the tasks
   * take some considerable time (use a loop inside f1 and f2)
   * @param f function to time
   */
  def timeFunction[A](f: () => A, taskName: String): A = {
    val t0 = System.nanoTime()
    val res = f()
    val t1 = System.nanoTime()
    println("Elapsed time (" + taskName + "): " + (t1 - t0) / 1000000000.0 + "ns")
    res
  }

}

