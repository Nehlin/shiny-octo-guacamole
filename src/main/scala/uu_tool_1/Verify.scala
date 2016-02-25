package uu_tool_1

import scala.collection.mutable.ArrayBuffer

/**
 * A result from a verification can be:
 * Safe - the bad state of the protocol is not reachable
 * Unsafe - the bad state of the protocol is reachable
 * Unclear - a Safe/Unsafe result could not be obtained before breaking due to
 *           reaching maximum allowed k.
 */
abstract class Result
case object Safe extends Result
case object Unsafe extends Result
case object Unclear extends Result

object Verify {

  def resultToString(result: Result): String = {
    result match {
      case Safe => "Safe"
      case Unsafe => "Unsafe"
      case Unclear => "Unclear"
    }
  }

  /**
   * Calls post and abstract post for increasing values of k until a conclusive result
   * is found or k exceeds maxK.
   *
   * NOTE: if more data is needed for logs and statistics, the should be saved here and
   * returned as a part of the return-tuple.
   *
   * @param protocol the protocol to verify
   * @param maxK maximum k value before breaking
   * @return A Result and the k value where this value was obtained.
   */
  def run(protocol: Protocol, maxK: Int): (Result, Int) = {

    // First test will be at k + 1
    var k = 1
    var result:Result = Unclear

    def containsBadState(configurations: Set[ArrayBuffer[Int]]): Boolean = {
      configurations.foldLeft(false)((res, config) => {
        if (res)
          res
        else {
          SubWord.test(config, protocol.badConfiguration)
        }
      })
    }

    /**
     * Tests the protocol for configurations of size k.
     *
     * @param k size of configurations to test for
     * @param preComputed optional resulting views from doing post on k. This is here
     *                    since testing abstract post for k computes normal post for
     *                    k+1, so this is a way to reuse this value. If this value is
     *                    present, it will be used, if not, it will be computed.
     * @return the result, of testing for k. If the result is conclusive (Safe, Unsafe)
     *         nothing else is computed. If Unclear, post for k+1 has been computed, so
     *         this result is returned, so it can be reused for the next iteration.
     */
    def testForK(k: Int, preComputed: Option[Set[ArrayBuffer[Int]]]): (Result, Option[Set[ArrayBuffer[Int]]]) = {
      val initialConfiguration = Set(protocol.initialConfiguration(k))
      val postConfigs = preComputed.getOrElse(Post.fixPoint(initialConfiguration, protocol.rules))
      if (containsBadState(postConfigs)) {
        (Unsafe, None)
      } else {
        val nextInitialConfiguration = Set(protocol.initialConfiguration(k + 1))
        val nextPostResult = Post.fixPoint(nextInitialConfiguration, protocol.rules)
        val aPostConfigs = AbstractPost.fixPoint(initialConfiguration, protocol.rules, nextPostResult)
        if (!containsBadState(aPostConfigs)) {
          (Safe, None)
        } else {
          (Unclear, Some(nextPostResult))
        }
      }
    }

    var preComputedPost: Option[Set[ArrayBuffer[Int]]] = None
    while(k < maxK && result == Unclear) {
      k += 1
      val (res, nextPost) = testForK(k, preComputedPost)
      result = res
      preComputedPost = nextPost
    }

    (result, k)
  }
}