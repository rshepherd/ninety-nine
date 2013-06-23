package ninetynine

object Problems {

  def main(args: Array[String]) = println("If you're having state problems, I feel bad for you son, " +
                                          "I've got 99 problems but side-effects ain't one.")

  // P01 (*) Find the last element of a list.
  def last[A](list: List[A]) : A = list.last

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]) : A = list.dropRight(1).last

  // P03 (*) Find the Kth element of a list.
  def nth[A](i: Int, list: List[A]) : A = list.drop(i).head

}



