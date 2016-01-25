object AbstractPost {
  def single(views: Set[Vector[Int]], rules: Set[Rule], roMap: Concretisation.RoMap, k: Int): Set[Vector[Int]] = {
    val concretisation = Concretisation.generate(views, k, roMap)
    val newConfigs = concretisation.map(cView => {
      Post.single(cView, rules)
    }).flatten

    Views.fromConfigurations(newConfigs, k)
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