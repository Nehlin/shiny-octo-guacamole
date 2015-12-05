import org.scalatest._

class ReachabilityTest extends FlatSpec with Matchers {

  val configurationSingle = new Configuration(Vector(1, 0, 0, 2, 0))
  val rulesSingle: Set[Rule] = Set(
    Unrestricted(0, 1),
    Existential(0, 2, Set(2), Left),
    Universal(2, 3, Set(0, 1), Right),
    Universal(2, 4, Set(0), Left),
    Universal(2, 5, Set(0, 1), Left)
  )
  val targetConfigurationsSingle = Set(
    new Configuration(Vector(1, 1, 0, 2, 0)),
    new Configuration(Vector(1, 0, 1, 2, 0)),
    new Configuration(Vector(1, 0, 0, 2, 1)),
    new Configuration(Vector(1, 0, 0, 2, 2)),
    new Configuration(Vector(1, 0, 0, 3, 0)),
    new Configuration(Vector(1, 0, 0, 5, 0))
  )

  val configurationsMultiple = Set(
    new Configuration(Vector(0, 0)),
    new Configuration(Vector(3, 4, 5))
  )
  val rulesMultiple: Set[Rule] = Set(
    Unrestricted(0, 1),
    Unrestricted(1, 2),
    Universal(5, 7, Set(4, 3), Left),
    Existential(3, 8, Set(5, 6), Right)
  )
  val targetConfigurationsMultiple = Set(
    new Configuration(Vector(0, 0)),
    new Configuration(Vector(0, 1)),
    new Configuration(Vector(0, 2)),
    new Configuration(Vector(1, 0)),
    new Configuration(Vector(1, 1)),
    new Configuration(Vector(1, 2)),
    new Configuration(Vector(2, 0)),
    new Configuration(Vector(2, 1)),
    new Configuration(Vector(2, 2)),
    new Configuration(Vector(3, 4, 5)),
    new Configuration(Vector(3, 4, 7)),
    new Configuration(Vector(8, 4, 5))
  )

  "Forward reachability" should "work for single configurations" in {
    Reachability.forwardsSingle(configurationSingle, rulesSingle) should be (targetConfigurationsSingle)
  }
  
  "Forward reachability" should "work for sets of configurations" in {
    Reachability.forwards(configurationsMultiple, rulesMultiple) should be (targetConfigurationsMultiple)
  }
}