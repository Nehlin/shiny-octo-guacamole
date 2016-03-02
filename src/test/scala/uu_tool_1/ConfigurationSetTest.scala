package uu_tool_1

import org.scalatest._
import scala.collection.mutable.{ListBuffer => MList}

class ConfigurationSetTest extends FlatSpec with Matchers {

  def listEqualToSet(mList: MList[Array[Int]], set: Set[Array[Int]]): Boolean = {
    mList.length == set.size && (mList.toSet[Array[Int]].map(_.toList) == set.map(_.toList))
  }

  def listEqual(mList1: MList[Array[Int]], mList2: MList[Array[Int]]): Boolean = {
    mList1.length == mList2.length &&
      mList1.toSet[Array[Int]].map(_.toList) == mList2.toSet[Array[Int]].map(_.toList)
  }

  def configsListsEqual(c1: ConfigurationSet, c2: ConfigurationSet): Boolean = {
    c1.configurations.length == c2.configurations.length &&
      c1.configurations.toSet[Array[Int]].map(_.toList) == c2.configurations.toSet[Array[Int]].map(_.toList)
  }

  val alphabetSize = 10
  val a1 = Array(1, 1, 1)
  val a2 = Array(1, 2, 2)
  val a3 = Array(3, 4, 5)
  val a4 = Array(7, 8 ,9)

  val s1 = new ConfigurationSet(alphabetSize)
  "Inserting" should "not create duplicates in configurations list" in {
    s1.configurations.size should be(0)
    s1.insert(a1)
    s1.configurations.size should be(1)
    s1.insert(a1)
    s1.configurations.size should be(1)
    s1.insert(a2)
    s1.configurations.size should be(2)
    s1.insert(a1)
    s1.insert(a2)
    s1.configurations.size should be(2)
  }

  val s2 = new ConfigurationSet(alphabetSize)
  "Inserted configurations" should "be possible to look up" in {
    s2.contains(a1) should be(false)
    s2.contains(a2) should be(false)
    s2.insert(a1)
    s2.contains(a1) should be(true)
    s2.contains(a2) should be(false)
    s2.insert(a2)
    s2.contains(a1) should be(true)
    s2.contains(a2) should be(true)
  }

  val s3 = new ConfigurationSet(alphabetSize)
  "Fast lookup configurations list and computed configurations list" should "be equal" in {
    listEqualToSet(s3.configurations, Set())
    listEqualToSet(s3.calculateConfigurations, Set())
    listEqual(s3.configurations, s3.calculateConfigurations)
    s3.insert(a1)
    listEqualToSet(s3.configurations, Set(a1))
    listEqualToSet(s3.calculateConfigurations, Set(a1))
    listEqual(s3.configurations, s3.calculateConfigurations)
    s3.insert(a2)
    listEqualToSet(s3.configurations, Set(a1, a2))
    listEqualToSet(s3.calculateConfigurations, Set(a1, a2))
    listEqual(s3.configurations, s3.calculateConfigurations)
  }


  val s4 = new ConfigurationSet(alphabetSize)
  s4.insert(a1)
  s4.insert(a2)
  s4.insert(a3)
  val s3c = s4.copy()
  "Copying a set" should "create an identical set" in {
    s4.alphabetSize should be(s3c.alphabetSize)
    configsListsEqual(s4, s3c) should be(true)
    listEqual(s4.calculateConfigurations, s3c.calculateConfigurations) should be(true)
  }

  val s5 = new ConfigurationSet(alphabetSize)
  s5.insert(a2)
  val a2modPos = 0
  val a2modVal = 4
  val a2mod = a2.clone()
  a2mod(a2modPos) = a2modVal
  val s5c1 = s5.copy()
  val s5c2 = s5.copy()
  "Copied set" should "contain separate data from original set" in {
    listEqualToSet(s5.configurations, Set(a2)) should be(true)
    listEqualToSet(s5c1.configurations, Set(a2)) should be(true)
    listEqualToSet(s5c2.configurations, Set(a2)) should be(true)
    s5c1.configurations.head(a2modPos) = a2modVal
    listEqualToSet(s5.configurations, Set(a2)) should be(true)
    listEqualToSet(s5c1.configurations, Set(a2mod)) should be(true)
    s5c2.insert(a4)
    listEqualToSet(s5.configurations, Set(a2)) should be(true)
    listEqualToSet(s5c2.configurations, Set(a2, a4)) should be(true)
  }

  val s6 = new ConfigurationSet(alphabetSize)
  s6.insert(a1)
  s6.insert(a2)
  s6.insert(a3)

  val s7 = new ConfigurationSet(alphabetSize)
  s7.insert(a2)
  s7.insert(a4)
  s7.insert(a3)

  val s8 = new ConfigurationSet(alphabetSize)
  s8.insert(a1)

  val s9 = new ConfigurationSet(alphabetSize)
  s9.insert(a2)
  
  val sEmpty = new ConfigurationSet(alphabetSize)

  "Set intersection" should "be work and be symmetric" in {
    configsListsEqual(s6.intersection(s7), s7.intersection(s6)) should be(true)
    listEqualToSet(s6.intersection(s7).configurations, Set(a2, a3)) should be(true)
    listEqualToSet(s7.intersection(s6).configurations, Set(a2, a3)) should be(true)

    configsListsEqual(s8.intersection(s9), s9.intersection(s8)) should be(true)
    listEqualToSet(s8.intersection(s9).configurations, Set())
  }

  "Intersecting a set with itself" should "create a copy of the set" in {
    configsListsEqual(s6.intersection(s6), s6) should be(true)
  }

  "Intersecting a set with the empty set" should "create an empty set" in {
    s6.intersection(sEmpty).configurations.length should be(0)
  }


  "Set union" should "work and be symmetric" in {
    configsListsEqual(s6.union(s7), s7.union(s6)) should be(true)
    listEqualToSet(s6.union(s7).configurations, Set(a1, a2, a3, a4)) should be(true)
    listEqualToSet(s7.union(s6).configurations, Set(a1, a2, a3, a4)) should be(true)

    configsListsEqual(s8.union(s9), s9.union(s8)) should be(true)
    listEqualToSet(s8.union(s9).configurations, Set(a1, a2)) should be(true)
  }

  "Creating the union of a set and itself" should "create a copy of the set" in {
    configsListsEqual(s6.union(s6), s6) should be(true)
  }

  "Creating the union of a set and the empty set" should "create a copy of the set" in {
    configsListsEqual(s7.union(sEmpty), s7) should be(true)
  }
}