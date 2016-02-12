import scala.io.Source

abstract class ProtocolOutput
case object Yes extends ProtocolOutput
case object No extends ProtocolOutput
case object Only extends ProtocolOutput

/**
 * Class for parsing and obtaining the user input from the command-line and from
 * config.txt
 *
 * Once the class is created, use the get-methods to obtain specific values.
 *
 * To add a new value, do the following:
 * 1) add a private Parameter instance
 * 2) add a case in the parse function
 * 3) add a public get-method
 *
 * @param args command line arguments
 */
class Parameters(args: Array[String]) {
  class Parameter(n: String, r: Boolean, d: Any) {
    val name = n
    val required = r
    val default = d
  }

  private val ProtocolFile = new Parameter("p", true, ())
  private val MaxK = new Parameter("max_k", false, 5)
  private val ProtocolDot = new Parameter("protocol_dot", false, No)
  private val CounterDot = new Parameter("counter_dot", false, false)
  private val LogFile = new Parameter("log_file", false, true)

  private val commandList = List(ProtocolFile, MaxK, ProtocolDot, CounterDot, LogFile)
  private val commandMap = commandList.map(cmd => (cmd.name, cmd)).toMap

  /**
   * Takes a key and a value and returns a key-value pair that has the same key
   * but where the value is converted into a meaningful representation (for
   * example, if the value is an integer, the value parameter is converted from
   * its string representation into an Integer)
   *
   * Returns None if an unknown value is present
   *
   * @param key key used in the command line and config-file.
   * @param value string representation of the value
   * @return an option that contains a parsed version of the pair if the key is
   *         known or None if not
   */
  private def parse(key: String, value: String): Option[(String, Any)] = {
    val parsedVal = key match {
      case "p" => Some(value)
      case "max_k" => Some(value.toInt)
      case "protocol_dot" =>
        val po = value.toLowerCase match {
          case "true" => Yes
          case "only" => Only
          case _ => No
        }
        Some(po)
      case "counter_dot" => Some(value.toBoolean)
      case "log_file" => Some(value.toBoolean)
      case _ => None
    }

    parsedVal match {
      case Some(v) => Some(key, v)
      case None => None
    }
  }

  /**
   * Parses the config file. This means removing comments and whitespace, and splitting
   * the key-value pairs. The rest of the job is done by the parse-function
   *
   * @param fileName name of config-file.
   * @return a map corresponding to the config-file, where every key points to a parsed value
   */
  private def parseFile(fileName: String): Map[String, Any] = {
    val rawLines = Source.fromFile(fileName).getLines().toList
    val trimmedLines = rawLines.map(line => line.trim)
    val validLines = trimmedLines.filter(line => line != "" && !line.startsWith("%"))
    val kvPairs = validLines.map(line => {
      val split = line.split("=")
      (split(0).trim, split(1).trim)
    })
    kvPairs.map{case (key, value) => parse(key, value)}.flatten.toMap
  }

  /**
   * Parses the input from the command line. All values in the command line have to appear
   * in pairs of the form -key value.
   *
   * @param args list of command line arguments
   * @return a map corresponding to the command line, where every key points to a parsed value
   */
  private def parseCommandLine(args: Array[String]): Map[String, Any] = {
    args.grouped(2).map(arr => {
      val key = arr(0).stripPrefix("-")
      val value = arr(1)
      parse(key, value)
    }).flatten.toMap
  }

  private val fileMap = parseFile("config.txt")
  private val commandLineMap = parseCommandLine(args)

  /**
   * Retrieves a value from the protocol. When fetching a value that is present both
   * in the config-file and through the command line, command line has precedence.
   *
   * If a parameter is not present but is not required, it's default value (all non-
   * required parameters must have default values) is returned. If a required parameter
   * is not present, an exception is thrown.
   *
   * @param argName name of argument to fetch. This should be equal to Parameter.name
   *                for some Parameter
   * @return the value of the parameter, from the command line, the config-file or
   *         default value, in that priority.
   */
  private def get(argName: String): Any = {
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
  def getProtocolFile: String = get(ProtocolFile.name).asInstanceOf[String]
  def getMakeProtocolDot: ProtocolOutput = get(ProtocolDot.name).asInstanceOf[ProtocolOutput]
  def getMakeCounterDot: Boolean = get(CounterDot.name).asInstanceOf[Boolean]
  def getMakeLogFile: Boolean = get(LogFile.name).asInstanceOf[Boolean]
  def getMaxK: Int = get(MaxK.name).asInstanceOf[Int]

}
