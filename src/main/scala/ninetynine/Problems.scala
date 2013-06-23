package ninetynine

object Problems {

  def main(args: Array[String]) = println("If you're having side-effect problems I feel bad for you son, " +
                                          "I've got 99 problems but mutation ain't one.")

  // P01 (*) Find the last element of a list.
  def last[A](list: List[A]) = list.last

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]) = list.dropRight(1).last

  // P03 (*) Find the Kth element of a list.
  def nth[A](i: Int, list: List[A]) = list.drop(i).head

  // P04 (*) Find the number of elements of a list.
  def len[A](list: List[A]) = list.foldLeft(0) { (a, _) => a + 1 }

  // P05 (*) Reverse a list.
  def rev[A](list: List[A]) = list.foldLeft(List[A]()) { (list, h) => h :: list }

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
  def compress[A](list: List[A]) = list.foldLeft(List[A]()) { (l, e) =>
    if (l.isEmpty || l.head != e) e :: l  else l
  }.reverse

}



