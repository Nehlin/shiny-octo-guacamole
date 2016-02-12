import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

/**
 * Simple wrapper to write strings to files. Also includes constants for default
 * file names.
 */
object FileWriter {
  val OutputDir = "result_files/"
  val LogFile = OutputDir + "Log.txt"
  val ProtocolDotFile = OutputDir + "Protocol.dot"
  val CounterDotFile = OutputDir + "CounterExample.dot"

  /**
   * Creates the output directory if it does not already exist.
   */
  def createOutputDirIfNeeded():Unit = {
    if (!Files.exists(Paths.get(OutputDir))) {
      new File(OutputDir).mkdirs()
    }
  }

  /**
   * Writes data to the file named fileName. Creates the default output directory
   * if it does not already exist.
   *
   * @param data string containing data to save
   * @param fileName name of file to save to
   */
  def write(data: String, fileName: String): Unit = {
    createOutputDirIfNeeded()
    val filePath = fileName
    Files.write(Paths.get(filePath), data.getBytes(StandardCharsets.UTF_8))
  }
}