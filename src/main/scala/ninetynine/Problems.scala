package ninetynine

object Problems {

  def main(args: Array[String]) = println("If you're having side-effect problems I feel bad for you son, " +
                                          "I've got 99 problems but mutation ain't one.")

  // P01 (*) Find the last element of a list.
  def last[A](list: List[A]) : A = list.last

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]) : A = list.dropRight(1).last

  // P03 (*) Find the Kth element of a list.
  def nth[A](i: Int, list: List[A]) : A = list.drop(i).head

  // P04 (*) Find the number of elements of a list.
  def len[A](list: List[A]) = list.foldLeft(0) { (a, _) => a + 1 }

  // P05 (*) Reverse a list.
  def rev[A](list: List[A]): List[A] = list.foldLeft(List[A]()) { (list, h) => h :: list }
}



