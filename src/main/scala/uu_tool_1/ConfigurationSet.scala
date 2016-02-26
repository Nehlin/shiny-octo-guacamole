package uu_tool_1

/**
 * One of the big bottlenecks of Scala is the slowness of scala data structures
 * compared to raw Java arrays. Scala ArrayBuffers allow insertion and lookup in
 * sets which Arrays does not, and this is crucial in many parts of the program.
 *
 * This file contains classes that implement sets holding configurations as Java-
 * arrays. Currently it supports creation, insertion, lookup and copying.
 *
 * NOTE: this data structure does not handle configurations of different sizes
 * well. Inserting the configuration (1, 2, 3) will make a lookup for (1, 2)
 * return true. This should not be a problem since its easy to use separate sets
 * for different configuration sizes.
 *
 * Before this can be used as an alternative to sets of ArrayBuffers, two non-
 * trivial methods need to be implemented:
 * - Set union. An efficient way to compute the union of two ConfigurationSets
 * - Iterating all configurations of size n. For ConfigurationSets to be useful
 * there needs to be a convenient way of iterating through all its stored
 * configurations.
 */

class ConfigurationSet(aSize: Int, cs: Array[Any]) {

  val alphabetSize = aSize
  val cSet:Array[Any] = cs

  /**
   * This is the constructor that should be used. The two-parameter constructor is
   * only supposed to be used for copying.
   *
   * @param aSize number of possible states to store. This should be equal to
   *              Protocol.alphabetSize
   */
  def this(aSize: Int) = {
    this(aSize, new Array(aSize))
  }

  private def next(configSet: Array[Any], currentState: Int): Array[Any] = {
    configSet(currentState).asInstanceOf[Array[Any]]
  }

  private def insert_(configuration: Array[Int],
                      configSet: Array[Any],
                      index: Int): Unit = {

    if (index < configuration.length) {
      val currentState = configuration(index)

      if (configSet(currentState) == null) {
        configSet(currentState) = new Array(alphabetSize)
      }
      insert_(configuration, next(configSet, currentState), index + 1)
    }
  }

  /**
   * Inserts a configuration into the set
   *
   * @param configuration configuration to insert
   */
  def insert(configuration: Array[Int]): Unit = {
    insert_(configuration, cSet, 0)
  }

  private def lookup_(configuration: Array[Int], configSet: Array[Any], index: Int): Boolean ={
    if (index == configuration.length) {
      true
    } else {
      val currentState = configuration(index)
      if (configSet(currentState) == null) {
        false
      } else {
        lookup_(configuration, next(configSet, currentState), index + 1)
      }
    }
  }

  /**
   * Looks up a configuration in a set
   *
   * @param configuration configuration to do lookup on
   * @return true iff configuration is contained in this set.
   */
  def lookup(configuration: Array[Int]): Boolean = {
    lookup_(configuration, cSet, 0)
  }

  /**
   * Not really a proper toString-method, just prints the first node, rather than whole the tree.
   * @return
   */
  override def toString = {
    if (cSet == null) {
      "_"
    } else {
      val arr = cSet.asInstanceOf[Array[Any]]
      "[" + (0 until arr.length).map(index =>  if (arr(index) == null) " " else "\u25CF").mkString("][") + "]"
    }
  }

  /**
   * Deep copy of the arrays.
   *
   * @param configSet the array representation of the set
   * @return a deep copy of configSet
   */
  private def copyArr(configSet: Array[Any]): Array[Any] = {
    if (configSet != null) {
      val csCopy = configSet.clone()
      for (i <- 0 until configSet.length) {
        csCopy(i) = copyArr(next(configSet, i))
      }
      csCopy
    } else {
      null
    }
  }

  /**
   * Creates a deep copy
   *
   * @return a deep copy of this Configuration set.
   */
  def copy(): ConfigurationSet = {
    val copiedArr = copyArr(cSet)
    new ConfigurationSet(alphabetSize, copiedArr)
  }

}