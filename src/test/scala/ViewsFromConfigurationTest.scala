import org.scalatest._

class ViewsFromConfigurationTest extends FlatSpec with Matchers {
  var c1 = Vector(0)
  var c2 = Vector(1, 2, 3)
  var c3 = Vector(4, 4, 4)
  var c4 = Vector(6, 7, 8, 9, 10)
  var c5 = Vector(1, 4, 3, 4)

  "Views" should "be created from configurations of size 1" in {
    ViewsFromConfiguration.make(c1, 0) should be (Set())
    ViewsFromConfiguration.make(c1, 1) should be (Set(Vector(0)))
  }

  "Views" should "be created from configurations of larger sizes" in {
    ViewsFromConfiguration.make(c2, 0) should be (Set())
    ViewsFromConfiguration.make(c2, 1) should be (Set(Vector(1), Vector(2), Vector(3)))
    ViewsFromConfiguration.make(c2, 2) should be (Set(Vector(1, 2), Vector(1, 3), Vector(2, 3)))
    ViewsFromConfiguration.make(c2, 3) should be (Set(Vector(1, 2, 3)))
    ViewsFromConfiguration.make(c4, 3) should be (Set(
      Vector(6, 7, 8),
      Vector(6, 7, 9),
      Vector(6, 7, 10),
      Vector(6, 8, 9),
      Vector(6, 8, 10),
      Vector(6, 9, 10),
      Vector(7, 8, 9),
      Vector(7, 8, 10),
      Vector(7, 9, 10),
      Vector(8, 9, 10)
    ))
  }

  "Views" should "not create duplicates" in {
    ViewsFromConfiguration.make(c3, 2) should be (Set(Vector(4, 4)))
    ViewsFromConfiguration.make(c5, 2) should be (Set(
      Vector(1, 4),
      Vector(1, 3),
      Vector(4, 3),
      Vector(4, 4),
      Vector(3, 4)
    ))
  }

  "Views" should "be created from multiple configurations" in {
    ViewsFromConfiguration.makeMultiple(Set(c4, c5), 4) should be (Set(
      Vector(7, 8, 9, 10),
      Vector(6, 8, 9, 10),
      Vector(6, 7, 9, 10),
      Vector(6, 7, 8, 10),
      Vector(6, 7, 8, 9),
      Vector(1, 4, 3, 4)
    ))
    ViewsFromConfiguration.makeMultiple(Set(c1, c3, c4), 1) should be (Set(
      Vector(0),
      Vector(4),
      Vector(6),
      Vector(7),
      Vector(8),
      Vector(9),
      Vector(10)
    ))
  }
}