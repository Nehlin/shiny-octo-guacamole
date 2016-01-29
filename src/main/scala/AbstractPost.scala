import Concretisation.RoMap
import Configuration.Identifier
import collection.mutable.{ Set => MSet }

object AbstractPost {
  /*def single(views: Set[Vector[Int]], rules: Set[Rule], roMap: RoMap, k: Int): Set[Vector[Int]] = {

    val concretisation = Concretisation.generate(views, k, roMap)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurations(newConfigs, k)
  }*/

  def fixPoint_(views: Set[Vector[Int]],
                rules: Set[Rule],
                k: Int,
                roMap: RoMap,
                aLen: Int,
                existing: MSet[Identifier]): Set[Vector[Int]] = {

    Concretisation.addToRoMap(views, roMap)

    val concretisations = Concretisation.generate(views, roMap, aLen, existing)
    val cNext = concretisations.map(c => Post.single(c, rules)).flatten
    val v2 = Views.fromConfigurations(cNext, k)

    val newViews = v2 &~ views
    newViews
  }

  def fixPoint(views: Set[Vector[Int]],
               rules: Set[Rule],
               k: Int,
               aLen: Int): Set[Vector[Int]] = {

    val roMap = Concretisation.makeRoMap(views)
    val existing = MSet[Identifier]()
    views.foreach(v => existing += Configuration.makeIdentifier(v, aLen))

    fixPoint_(views, rules, k, roMap, aLen, existing)
  }

  // NOTE: For testing purposes only. Slow!
  def singleNaive(views: Set[Vector[Int]], rules: Set[Rule], k: Int): Set[Vector[Int]] = {
    val concretisation = Concretisation.naive(views, k)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurations(newConfigs, k)
  }
}