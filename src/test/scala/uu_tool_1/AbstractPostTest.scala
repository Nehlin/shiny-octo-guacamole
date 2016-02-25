package uu_tool_1

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable.ArrayBuffer

/**
 * The test cases for the abstract post are not exhaustive. To verify the abstract
 * post, the best idea is to compare it its naive implementation.
 *
 * NOTE: When doing this comparison, remember that the single posts differ slightly
 * between naive and fast implementation (naive may return source set, fast does not).
 */
class AbstractPostTest extends FlatSpec with Matchers {
  val r1 = Set[Rule](
    Existential(2, 4, Set(1, 3), Both)
  )

  val r2 = Set[Rule](
    Existential(2, 4, Set(1, 3), Both),
    Existential(3, 5, Set(1), Left),
    Universal(5, 6, Set(1, 4), Left)
  )

  // Should create (1, 2, 3) -> (1, 4, 3) -> (1, 4), (4, 3). (the view (1, 3) is already known.
  val s1 = Set(
    ArrayBuffer(1, 2),
    ArrayBuffer(1, 3),
    ArrayBuffer(2, 3)
  )

  "Abstract post" should "work for a single step" in {
    AbstractPost.singleStep(s1, r1, Set())._1 should be (Set(
      ArrayBuffer(4, 3),
      ArrayBuffer(1, 4)
    ))
    // singleStep differs slightly from naive version by omitting input views, so for a
    // working comparison, make union with original set.
    (s1 | AbstractPost.singleStep(s1, r1, Set())._1) should be (s1 | AbstractPost.singleStepNaive(s1, r1, 2))
  }

  "Abstract post" should "work for iterated posts" in {
    AbstractPost.fixPoint(s1, r2, Set()) should be (Set(
      ArrayBuffer(1, 2),
      ArrayBuffer(1, 3),
      ArrayBuffer(1, 4),
      ArrayBuffer(1, 5),
      ArrayBuffer(1, 6),
      ArrayBuffer(2, 3),
      ArrayBuffer(2, 5),
      ArrayBuffer(4, 3),
      ArrayBuffer(4, 5),
      ArrayBuffer(4, 6)
    ))
  }
}