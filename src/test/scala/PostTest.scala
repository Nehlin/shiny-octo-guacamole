import org.scalatest._
import scala.collection.mutable.ArrayBuffer

class PostTest extends FlatSpec with Matchers {

  val configurationSingle = ArrayBuffer(1, 0, 0, 2, 0)
  val rulesSingle: Set[Rule] = Set(
    Unrestricted(0, 1),
    Existential(0, 2, Set(2), Left),
    Universal(2, 3, Set(0, 1), Right),
    Universal(2, 4, Set(0), Left),
    Universal(2, 5, Set(0, 1), Left)
  )
  val targetConfigurationsSingle = Set(
    ArrayBuffer(1, 1, 0, 2, 0),
    ArrayBuffer(1, 0, 1, 2, 0),
    ArrayBuffer(1, 0, 0, 2, 1),
    ArrayBuffer(1, 0, 0, 2, 2),
    ArrayBuffer(1, 0, 0, 3, 0),
    ArrayBuffer(1, 0, 0, 5, 0)
  )

  val configurationsMultiple = Set(
    ArrayBuffer(0, 0),
    ArrayBuffer(3, 4, 5)
  )
  val rulesMultiple: Set[Rule] = Set(
    Unrestricted(0, 1),
    Unrestricted(1, 2),
    Universal(5, 7, Set(4, 3), Left),
    Existential(3, 8, Set(5, 6), Right)
  )
  val targetConfigurationsMultiple = Set(
    ArrayBuffer(0, 0),
    ArrayBuffer(0, 1),
    ArrayBuffer(0, 2),
    ArrayBuffer(1, 0),
    ArrayBuffer(1, 1),
    ArrayBuffer(1, 2),
    ArrayBuffer(2, 0),
    ArrayBuffer(2, 1),
    ArrayBuffer(2, 2),
    ArrayBuffer(3, 4, 5),
    ArrayBuffer(3, 4, 7),
    ArrayBuffer(8, 4, 5)
  )

  "Forward reachability" should "work for single configurations" in {
    Post.single(configurationSingle, rulesSingle) should be (targetConfigurationsSingle)
  }
  
  "Forward reachability" should "work for sets of configurations" in {
    Post.fixPoint(configurationsMultiple, rulesMultiple) should be (targetConfigurationsMultiple)
  }
}