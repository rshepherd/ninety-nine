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

  }

}
