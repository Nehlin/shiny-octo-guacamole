package uu_tool_1

import uu_tool_1.Configuration.Config

import scala.collection.mutable.{Set => MSet, ArrayBuffer}

object Views {
  /**
   * Breaks up a configuration into views.
   *
   * @param configuration configuration to break into views.
   * @param size size of views. Only views of this size will be created.
   *             Has to be defined if includeAll is false.
   * @param includeAll if this is set, size will be ignored and all views will be created.
   * @return a set of views of a set size, or if includeAll is set, all views of a configuration
   */
  private def general(configuration: Config, size: Option[Int], includeAll: Boolean): Set[Config] = {

    def binarySum_(num: Int, sum: Int): Int = {
      if (num == 1) {
        1 + sum
      } else {
        val current = if (num % 2 == 1) 1 else 0
        binarySum_(num / 2, sum + current)
      }
    }

    /**
     * Returns the sum of all bits that are 1 in the binary representation of num.
     *
     * @param num positive integer. 0 or negative integers causes function to get stuck in infinite loop.
     * @example binarySum(37) = 3 // 37 = 100101
     * @example binarySum(32) = 1 // 37 = 100000
     */
    def binarySum(num: Int): Int = binarySum_(num, 0)


    /**
     * Returns the sub-array of array that is specified by the binary representation of bin, in a right to left fashion
     *
     * While this function is not very intuitive, it is reasonably fast.
     *
     * @param word original array
     * @param bin integer whose binary representation selects which elements to select for sub-array
     * @example subWord(Array("A", "B", "C", "D", "E"), 1) = Array("A") // 1 = 00001 => 10000
     * @example subWord(Array("A", "B", "C", "D", "E"), 11) = Array("A", "B", "D") //11 = 01011 => 11010
     */
    def subWord(word: Config, bin: Int): Config = {
      val length = binarySum(bin)
      val res = ArrayBuffer.fill(length)(0)
      var insertPos = 0
      var index = 0
      while(insertPos < length) {
        val isSet = (bin & (1 << index)) != 0
        if (isSet) {
          res(insertPos) = word(index)
          insertPos = insertPos + 1
        }
        index = index + 1
      }
      res
    }

    val configLength = configuration.length

    val views = for (
      i <- 1 until 1 << configLength
      if includeAll || binarySum(i) == size.get
    ) yield subWord(configuration, i)

    views.toSet
  }

  /**
   * Creates all views of a certain size from a configuration
   * @param configuration original configuration
   * @param size size of created views
   * @return all views of size from configuration
   */
  def fromConfiguration(configuration: Config, size: Int): Set[Config] = general(configuration, Some(size), false)

  /**
   * Creates all views from a configuration
   * @param configuration original configuration
   * @return all views of all sizes from configuration
   */
  def allFromConfiguration(configuration: Config): Set[Config] = general(configuration, None, true)

  /**
   * Creates all views of a certain size from a set of configurations
   * @param configurations set of original configurations
   * @param size size of created views
   * @return all views of size from the configurations
   */
  def fromConfigurations(configurations: Set[Config], size: Int): Set[Config] = {
    configurations.foldLeft(Set[Config]())((acc, config) => acc ++ fromConfiguration(config, size))
  }

  /**
   * Creates the views from configuration that are 1 smaller in size than the
   * original configuration, unless these new views are already known
   *
   * NOTE: uses mutable sets to avoid conversions between mutable and immutable.
   *
   * @param configuration original configuration
   * @param existing optional set of existing views that should be ignored
   * @return All views of configuration that are of size n-1 where n is the
   *         size of configuration, unless those views that are present in
   *         existing
   */
  def fromConfigurationFixed(configuration: Config,
                             existing: Option[MSet[Config]]): MSet[Config] = {


    var excludedState = configuration.head
    val currentView = configuration.tail
    def updateCurrentView(pos: Int): Config = {
      val pm1 = pos - 1
      if (pm1 >= 0) {
        val newExcluded = currentView(pm1)
        currentView(pm1) = excludedState
        excludedState = newExcluded
      }
      currentView
    }

    val res = MSet[Config]()

    (0 until configuration.length).foreach(index => {
      updateCurrentView(index)
      if (!existing.isDefined || !existing.get.contains(currentView)) {
        res.add(currentView.clone())
      }
    })

    res
  }

  /**
   * Creates the views from a set of configuration that are 1 smaller in size
   * than the original configurations, unless these new views are already known
   *
   * @param configurations original configurations. All configurations are
   *                       expected to be of the same size.
   * @param existing optional set of existing views that should be ignored
   * @return All views of the configurations that are of size n-1 where n is the
   *         size of configuration, unless those views that are present in
   *         existing
   */
  def fromConfigurationsFixed(configurations: Set[Config],
                              existing: Option[MSet[Config]]): Set[Config] = {

    configurations.map(configuration => fromConfigurationFixed(configuration, existing)).flatten
  }
}
