object SubWord {
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
