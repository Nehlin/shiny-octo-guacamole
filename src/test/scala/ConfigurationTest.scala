import org.scalatest._
import scala.collection.mutable.ArrayBuffer

class ConfigurationTest extends FlatSpec with Matchers {

  val c = ArrayBuffer(0, 1, 2, 3, 4)

  "Configurations" should "know leftOf and rightOf" in {
    Configuration.leftOf(c, 0) should be(ArrayBuffer())
    Configuration.leftOf(c, 2) should be(ArrayBuffer(0, 1))
    Configuration.leftOf(c, 4) should be(ArrayBuffer(0, 1, 2, 3))
    Configuration.rightOf(c, 0) should be(ArrayBuffer(1, 2, 3, 4))
    Configuration.rightOf(c, 2) should be(ArrayBuffer(3, 4))
    Configuration.rightOf(c, 4) should be(ArrayBuffer())
  }

  val c1 = ArrayBuffer(1, 1)
  val c2 = ArrayBuffer(1, 2)
  val c3 = ArrayBuffer(5)
  val c4 = ArrayBuffer(0, 2)
  "Configurations" should "have an ordering" in {
    Configuration.compare(c1, c2) should be(-1)
    Configuration.compare(c1, c1) should be(0)
    Configuration.compare(c1, c3) should be(1)
    Configuration.compare(c1, c4) should be(1)
  }

}