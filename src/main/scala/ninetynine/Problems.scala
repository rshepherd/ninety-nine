package ninetynine

object Problems {

  // P01 (*) Find the last element of a list.
  def last[A](list: List[A]) = list.last

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]) = list.init.last

  // P03 (*) Find the Kth element of a list.
  def nth[A](i: Int, list: List[A]) = list.drop(i).head

  // P04 (*) Find the number of elements of a list.
  def len[A](list: List[A]) = list.foldLeft(0) { (a, _) => a + 1 }

  // P05 (*) Reverse a list.
  def rev[A](list: List[A]) = list.foldLeft(List[A]()) { (l, a) => a :: l }

  // P06 (*) Find out whether a list is a palindrome.
  def palindrome[A](list: List[A]) = {
    def compare(l1: List[A], l2: List[A]): Boolean = {
      if (l1.isEmpty) true
      else if (l1.head != l2.head) false
      else compare(l1.tail, l2.tail)
    }
    compare(list, list.reverse)
  }

  // P07 (**) Flatten a nested list structure.
  def flatten(lists: List[Any]) : List[Any] = lists.flatMap {
    case l: List[Any] => flatten(l)
    case a: Any => List(a)
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  def compress[A](list: List[A]) = list.foldLeft(List[A]()) { (l, a) =>
    if (l.isEmpty || l.head != a) a :: l else l
  }.reverse

  // P09 (**) Pack consecutive duplicates of list elements into sub-lists.
  def pack[A](list: List[A]): List[List[A]] = list.span(_ == list.head) match {
    case (dupes, Nil) => List(dupes)
    case (dupes, rest) => dupes :: pack(rest)
  }

  // P10 (*) Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] = {
    pack(list).foldLeft(List[(Int, A)]()) {
      (ll, l) => (l.size, l.head) :: ll
    }.reverse
  }


}





