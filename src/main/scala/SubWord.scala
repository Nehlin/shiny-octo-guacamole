object SubWord {

  /**
   * Tests if subWordCandidate is a (non-contiguous) sub word of word
   *
   * @param word full word
   * @param subWordCandidate candidate that may or may not be present inside word
   * @tparam T type of lists. The sub word relation is defined for lists of any
   *           type supporting equality.
   * @return true iff subWordCandidate is a sub word of word
   */
  def test[T](word: List[T], subWordCandidate: List[T]): Boolean = {
    if (subWordCandidate.isEmpty) {
      true
    } else if (word.isEmpty) {
      false
    } else {
      if (word.head == subWordCandidate.head) {
        test(word.tail, subWordCandidate.tail)
      } else {
        test(word.tail, subWordCandidate)
      }
    }
  }
}
