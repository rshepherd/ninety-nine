package ninetynine

import Problems._
import org.specs2.mutable._

class ProblemsSpec extends Specification {

  "My solution" should {

    "find the last element of a list. (P01)" in {
      last(List(1, 1, 2, 3, 5, 8)) must_== 8
    }

    "find the last but one element of a list. (P02)" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must_== 5
    }

    "find the Kth element of a list. (P03)" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) must_== 2
    }

    "find the number of elements of a list. (P04)" in {
      len(List(1, 1, 2, 3, 5, 8)) must_== 6
    }

    "reverse a list. (P05)" in {
      rev(List(1, 1, 2, 3, 5, 8)) must_== List(8, 5, 3, 2, 1, 1)
    }

    "find out if a list is a palindrome. (P06)" in {
      palindrome(List(1, 2, 3, 2, 1)) must beTrue
      palindrome(List(1, 2, 2, 1)) must beTrue
      palindrome(List(1, 2, 2, 1, 2)) must beFalse
    }

    "flatten a nested structure. (P07)" in {
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must_== List(1, 1, 2, 3, 5, 8)
    }

    "eliminate consecutive duplicates of list elements. (P08)" in {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== List('a, 'b, 'c, 'a, 'd, 'e)
    }

    "pack consecutive duplicates of list elements into sublists. (P09)" in {
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }

    "get the run-length encoding of a list. (P10)" in {
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

    }
  }

}
