package uu_tool_1

import scala.collection.mutable.{ListBuffer => MList}

/**
 * One of the big bottlenecks of Scala is the slowness of scala data structures
 * compared to raw Java ones. However, Scala ArrayBuffers allow insertion and
 * lookup in sets which Arrays does not, and this is crucial in many parts of
 * the program.
 *
 * This file contains classes that implement sets holding configurations as Java-
 * arrays. Currently it supports creation, insertion, lookup, copying, intersection
 * and extraction of all contained configurations.
 *
 * While this is faster than Scala sets of ArrayBuffers, the big performance
 * gain comes from being able to use Java Arrays rather than Scala ArrayBuffers
 * in the rest of the program.
 *
 * NOTE: this data structure does not handle configurations of different sizes
 * well. Inserting the configuration (1, 2, 3) will make a lookup for (1, 2)
 * return true. This should not be a problem since its easy to use separate sets
 * for different configuration sizes.
 *
 * NOTE: configurations can be obtained by a traversal of the internal tree
 * structure but this is costly and should be avoided whenever possible. To avoid
 * this, configurations are also stored in a ListBuffer. This causes some small
 * overhead on set creation and insertion, but offers big performance gains when
 * iterating members. A problem with this is that the same data is stored in two
 * places, so be careful whenever extending the functionality with methods that
 * edit the data.
 */

class ConfigurationSet(aSize: Int, cs: Array[Any]) {

  val alphabetSize = aSize
  val cTree:Array[Any] = cs
  // This does nothing on an empty cArr, so it's safe.
  val configurations:MList[Array[Int]] = calculateConfigurations

  /**
   * This is the constructor that should be used. The two-parameter constructor is
   * only supposed to be used internally.
   *
   * @param aSize number of possible states to store. This should be equal to
   *              Protocol.alphabetSize
   */
  def this(aSize: Int) = {
    this(aSize, new Array(aSize))
  }

  /**
   * Convenience function when moving to the next node in the internal tree
   * representation.
   *
   * NOTE: this does not test for null values. Do this before/after calling
   *
   * @param configSet current node in tree
   * @param index index of node in current tree
   * @return the tree contained in configSet(index), converted to an Array[Any]
   */
  private def next(configSet: Array[Any], index: Int): Array[Any] = {
    configSet(index).asInstanceOf[Array[Any]]
  }

  /**
   * Help function for the insertion
   *
   * @param configuration configuration to store
   * @param configSet current node in the internal tree
   * @param index index in configuration (current state)
   * @param didInsert true iff inserting this configuration into the tree has
   *                  already confirmed that it is a new insertion
   *                  
   * @return true iff the configuration was not already in the tree
   */
  private def insert_(configuration: Array[Int],
                      configSet: Array[Any],
                      index: Int,
                      didInsert: Boolean): Boolean = {

    if (index < configuration.length) {
      val currentState = configuration(index)

      val willInsert = configSet(currentState) == null
      if (willInsert) {
        configSet(currentState) = new Array(alphabetSize)
      }
      insert_(configuration, next(configSet, currentState), index + 1, willInsert || didInsert)
    } else {
      didInsert
    }
  }

  /**
   * Inserts a configuration into the set. This means inserting it into the tree
   * and (if it is not already present in the tree) into the list.
   *
   * @param configuration configuration to insert
   */
  def insert(configuration: Array[Int]): Unit = {
    val didInsert = insert_(configuration, cTree, 0, false)
    // This is necessary to avoid duplicates in configurations list.
    if (didInsert) {
      configurations += configuration.clone()
    }
  }

  // Add + as an alias for insert
  def +=(configuration: Array[Int]) = insert(configuration)

  /**
   * Help function for the lookup
   *
   * @param configuration configuration to search for
   * @param configSet current node in the internal tree
   * @param index index in configuration (current state)
   * @return true iff configuration exist in configSet
   */
  private def contains_(configuration: Array[Int], configSet: Array[Any], index: Int): Boolean ={
    if (index == configuration.length) {
      true
    } else {
      val currentState = configuration(index)
      if (configSet(currentState) == null) {
        false
      } else {
        contains_(configuration, next(configSet, currentState), index + 1)
      }
    }
  }

  /**
   * Looks up a configuration in a set
   *
   * @param configuration configuration to do lookup on
   * @return true iff configuration is contained in this set.
   */
  def contains(configuration: Array[Int]): Boolean = {
    contains_(configuration, cTree, 0)
  }

  /**
   * Not really a proper toString-method, just prints the first node, rather than whole the tree.
   * @return A bad string representation
   */
  override def toString = {
    if (cTree == null) {
      "_"
    } else {
      val arr = cTree.asInstanceOf[Array[Any]]
      "[" + (0 until arr.length).map(index =>  if (arr(index) == null) " " else "\u25CF").mkString("][") + "]"
    }
  }

  /**
   * Deep copy of the tree.
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
    val copiedArr = copyArr(cTree)
    new ConfigurationSet(alphabetSize, copiedArr)
  }

  /**
   * Help function for the intersection of two configuration sets. Computes the
   * intersection of the trees.
   * @param arr1 first tree
   * @param arr2 second tree
   * @return the tree that is the intersection of arr1 and arr2
   */
  def intersection_(arr1: Array[Any], arr2: Array[Any]): Array[Any] = {
    if (arr1 == null || arr2 == null) {
      null
    } else {
      val res = new Array[Any](alphabetSize)
      for (i <- 0 until alphabetSize) {
        res(i) = intersection_(next(arr1, i), next(arr2, i))
      }
      res
    }
  }

  /**
   * Creates the intersection of this set and another.
   *
   * @param otherSet set to intersect with this
   * @return a new set that is the intersection of this set and otherSet
   */
  def intersection(otherSet: ConfigurationSet): ConfigurationSet = {
    val intersectionArr = intersection_(this.cTree, otherSet.cTree)
    new ConfigurationSet(alphabetSize, intersectionArr)
  }

  // Add & as an alias for intersection
  def &(otherSet: ConfigurationSet): ConfigurationSet = intersection(otherSet)

  /**
   * Creates the union of this set and another.
   *
   * @param otherSet set to union with this
   * @return a new set that is the union of this set and otherSet
   */
  def union(otherSet: ConfigurationSet): ConfigurationSet = {
    val newSet = this.copy()
    for (configuration <- otherSet.configurations) {
      newSet.insert(configuration)
    }
    newSet
  }

  // Add | as an alias for union
  def |(otherSet: ConfigurationSet): ConfigurationSet = union(otherSet)

  /**
   * Help function for calculatesConfigurations. Adds found configurations to
   * accumulator.
   *
   * @param currentNode current node in the internal tree
   * @param path previously visited nodes in the traversal
   * @param acc mutable list of found configurations
   */
  private def addConfigurations_(currentNode: Array[Any], path: List[Int], acc: MList[Array[Int]]): Unit = {
    var isLeaf = true
    for (i <- 0 until alphabetSize) {
      if (currentNode(i) != null) {
        isLeaf = false
        addConfigurations_(next(currentNode,i), i::path, acc)
      }
    }
    if (isLeaf) {
      acc += path.reverse.toArray
    }
  }

  /**
   * Extracts all arrays contained in the internal tree
   *
   * NOTE: This is slow. Should not be used when iterating through the members
   * of the set. For that use, see the MList called 'configurations'
   *
   * @return a MList containing all configurations in the set.
   */
  def calculateConfigurations: MList[Array[Int]] = {
    val acc = MList[Array[Int]]()
    for (i <- 0 until alphabetSize) {
      if (cTree(i) != null) {
        addConfigurations_(next(cTree, i), List(i), acc)
      }
    }
    acc
  }
}