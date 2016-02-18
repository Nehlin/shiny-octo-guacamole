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

    def testForK(k: Int): Result = {
      val initialConfiguration = Set(protocol.initialConfiguration(k))
      val postConfigs = Post.fixPoint(initialConfiguration, protocol.rules)
      if (containsBadState(postConfigs)) {
        Unsafe
      } else {
        val aPostConfigs = AbstractPost.fixPoint(initialConfiguration, protocol.rules, k)
        if (!containsBadState(aPostConfigs)) {
          Safe
        } else {
          Unclear
        }
      }
    }

    while(k < maxK && result == Unclear) {
      k += 1
      result = testForK(k)
    }

    (result, k)
  }
}