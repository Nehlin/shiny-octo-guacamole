import collection.mutable.{ HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet }

object Concretisation {

  type RoMap = MHashMap[Vector[Int], MSet[Int]] with MMultiMap[Vector[Int], Int]

  // TODO: use vector views instead of .tail
  def makeMap(views: Set[Vector[Int]]): RoMap = {

    val mm = new MHashMap[Vector[Int], MSet[Int]] with MMultiMap[Vector[Int], Int]

    views.map(view => {
      val key = view.take(view.length - 1)
      mm.addBinding(key, view.last)
    })

    mm
  }

  // Naive concretisation function. For testing purposes only!
  def naive(views: Set[Vector[Int]], length: Int): Set[Vector[Int]] = {
    val alphabet = views.foldLeft(Set[Int]())((acc, curr) => acc ++ curr.toSet).toVector
    val aLen = alphabet.length

    val mSet = MSet[Vector[Int]]()

    def configForNum(num: Int, pos: Int): Vector[Int] = {
       if (pos == 0) {
         Vector(alphabet(num))
       } else {
         configForNum(num / aLen, pos - 1) ++ Vector(alphabet((num % aLen).toInt))
       }
    }

    (0 until math.pow(aLen.toDouble, (length).toDouble).toInt).foreach(num => {
      val candidate = configForNum(num, length - 1)
      val cViews = ViewsFromConfiguration.make(candidate, length - 1)
      if (cViews.subsetOf(views)) {
        mSet += candidate
      }
    })
    mSet.toSet
  }

  def g(views: Set[Vector[Int]], rightOf: RoMap): Set[Vector[Int]] = {
    def matchView(view: Vector[Int], pos: Int): Option[Set[Vector[Int]]] = {
      val beginning = view.take(view.length - 1)
      val end = view.drop(1)
      val last = view(view.length - 1)

      if (
        rightOf.isDefinedAt(beginning) &&
        rightOf(beginning).contains(last) &&
        rightOf.isDefinedAt(end)
      ) {
        val newViews = rightOf(end).map(rightState => {
          view :+ rightState
        })
        Option(newViews.toSet)
      } else {
        None
      }
    }

    for (view <- views) {
      println(matchView(view, 0))
    }

    views
  }

  def f(views: Set[Vector[Int]], existing: Set[Vector[Int]], map: MHashMap[Vector[Int], MSet[Int]] with MMultiMap[Vector[Int], Int]) = {
    val mSet = MSet[Vector[Int]]()

    for (view <- views) {
      println(view.tail)
      if (map.isDefinedAt(view.tail)) {
        println("hi")
        val newConfigs = map(view.tail).map(ending => view :+ ending)
        //mSet ++= newConfigs
        newConfigs.foreach(nc => {
          if (!existing.contains(nc)) {
            mSet += nc
          }
        })
      }
    }

    mSet.toSet
  }

}