import org.scalatest._

class InitialConfigurationsTest extends FlatSpec with Matchers {

  val nameToInternal = Map("Red" -> 0, "Green" -> 1, "Blue" -> 2, "Yellow" -> 3)
  val inputString1 = "(Red)*"
  val inputString2 = "(Red|Green)*"
  val inputString3 = "(Red|Green)(Blue|Yellow)*"
  val inputString4 = "(Blue|Yellow)"
  val inputString5 = "(Blue|Yellow)*(Red)*"

  val parsed1 = InitialConfigurations.parse(inputString1, nameToInternal)
  val parsed2 = InitialConfigurations.parse(inputString2, nameToInternal)
  val parsed3 = InitialConfigurations.parse(inputString3, nameToInternal)
  val parsed4 = InitialConfigurations.parse(inputString4, nameToInternal)
  val parsed5 = InitialConfigurations.parse(inputString5, nameToInternal)

  val vars1 = InitialConfigurations.varsFromRules(parsed1)
  val vars2 = InitialConfigurations.varsFromRules(parsed2)
  val vars3 = InitialConfigurations.varsFromRules(parsed3)
  val vars4 = InitialConfigurations.varsFromRules(parsed4)
  val vars5 = InitialConfigurations.varsFromRules(parsed5)

  val testString1 = InitialConfigurations.makeTestRegex(parsed1)
  val testString2 = InitialConfigurations.makeTestRegex(parsed2)
  val testString3 = InitialConfigurations.makeTestRegex(parsed3)
  val testString4 = InitialConfigurations.makeTestRegex(parsed4)
  val testString5 = InitialConfigurations.makeTestRegex(parsed5)

  "Initial configurations" should "be possible to generate from a single repeating variable" in {
    InitialConfigurations.make(0, vars1, testString1) should be(Set())
    InitialConfigurations.make(1, vars1, testString1) should be(Set(new Configuration(List(0))))
    InitialConfigurations.make(2, vars1, testString1) should be(Set(new Configuration(List(0, 0))))
  }
  "Initial configurations" should "be possible to generate from a multiple repeating variables" in {

    InitialConfigurations.make(0, vars2, testString2) should be(Set())
    InitialConfigurations.make(1, vars2, testString2) should be(
      Set(
        new Configuration(List(0)),
        new Configuration(List(1))
      )
    )
    InitialConfigurations.make(2, vars2, testString2) should be(
      Set(
        new Configuration(List(0, 0)),
        new Configuration(List(0, 1)),
        new Configuration(List(1, 0)),
        new Configuration(List(1, 1))
      )
    )
  }
  "Initial configurations" should "be possible to generate from constants followed by repetition" in {
    InitialConfigurations.make(3, vars3, testString3) should be(
      Set(
        new Configuration(List(0, 2, 2)),
        new Configuration(List(0, 2, 3)),
        new Configuration(List(0, 3, 2)),
        new Configuration(List(0, 3, 3)),
        new Configuration(List(1, 2, 2)),
        new Configuration(List(1, 2, 3)),
        new Configuration(List(1, 3, 2)),
        new Configuration(List(1, 3, 3))
      )
    )
  }
  "Initial configurations" should "be possible to generate from a single constant" in {
    InitialConfigurations.make(1, vars4, testString4) should be(
      Set(
        new Configuration(List(2)),
        new Configuration(List(3))
      )
    )
    InitialConfigurations.make(2, vars4, testString1) should be(Set())
  }

  "Initial configurations" should "be possible to generate from multiple repeating parts" in {
    InitialConfigurations.make(2, vars5, testString5) should be(
      Set(
        new Configuration(List(2, 2)),
        new Configuration(List(2, 3)),
        new Configuration(List(3, 2)),
        new Configuration(List(3, 3)),
        new Configuration(List(2, 0)),
        new Configuration(List(3, 0)),
        new Configuration(List(0, 0))
      )
    )
  }
}