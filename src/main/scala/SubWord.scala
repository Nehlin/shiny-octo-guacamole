import scala.collection.mutable.ArrayBuffer

object SubWord {

  /**
   * Tests if subWordCandidate is a (non-contiguous) sub word of fullWord
   *
   * @param fullWord full word
   * @param subWordCandidate candidate that may or may not be present inside word
   * @tparam T type of lists. The sub word relation is defined for lists of any
   *           type supporting equality.
   * @return true iff subWordCandidate is a sub word of word
   */
  def test[T](fullWord: ArrayBuffer[T], subWordCandidate: ArrayBuffer[T]): Boolean = {
    if (subWordCandidate.isEmpty) {
      true
    } else if (fullWord.isEmpty) {
      false
    } else {
      if (fullWord.head == subWordCandidate.head) {
        test(fullWord.tail, subWordCandidate.tail)
      } else {
        test(fullWord.tail, subWordCandidate)
      }
    }
  }
}
