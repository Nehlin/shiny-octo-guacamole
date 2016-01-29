import scala.io.Source

class Parameters(args: Array[String]) {
  class Parameter(n: String, r: Boolean, d: Any) {
    val name = n
    val required = r
    val default = d
  }

  val ProtocolFile = new Parameter("ProtocolFile", true, ())
  val MaxK = new Parameter("MaxK", false, 5)
  val ProtocolDot = new Parameter("ProtocolDot", false, false)
  val CounterDot = new Parameter("CounterDot", false, false)
  val LogFile = new Parameter("LogFile", false, true)

  val commandList = List(ProtocolFile, MaxK, ProtocolDot, CounterDot, LogFile)
  val commandMap = commandList.map(cmd => (cmd.name, cmd)).toMap

  def parse(key: String, value: String): Option[(String, Any)] = {
    key match {
      case "p" => Some(ProtocolFile.name, value)
      case "max_k" => Some(MaxK.name, value.toInt)
      case "protocol_dot" => Some(ProtocolDot.name, value.toBoolean)
      case "counter_dot" => Some(CounterDot.name, value.toBoolean)
      case "log_file" => Some(LogFile.name, value.toBoolean)
    }
  }

  def parseFile(fileName: String): Map[String, Any] = {
    val rawLines = Source.fromFile(fileName).getLines().toList
    val trimmedLines = rawLines.map(line => line.trim)
    val validLines = trimmedLines.filter(line => line != "" && !line.startsWith("%"))
    val kvPairs = validLines.map(line => {
      val split = line.split("=")
      (split(0).trim, split(1).trim)
    })
    kvPairs.map{case (key, value) => parse(key, value)}.flatten.toMap
  }

  def parseCommandLine(args: Array[String]): Map[String, Any] = {
    args.grouped(2).map(arr => {
      val key = arr(0).stripPrefix("-")
      val value = arr(1)
      parse(key, value)
    }).flatten.toMap
  }

  val fileMap = parseFile("config.txt")
  val commandLineMap = parseCommandLine(args)

  def get(argName: String): Any = {
    if (commandLineMap.contains(argName)) {
      commandLineMap(argName)
    } else if (fileMap.contains(argName)) {
      fileMap(argName)
    } else {
      val parameter = commandMap(argName)
      if (parameter.required) {
        throw new Exception("Missing required parameter: " + argName)
      } else {
        parameter.default
      }
    }
  }

}
