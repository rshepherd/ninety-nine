package ninetynine

import Problems._
import org.specs2.mutable._

class ProblemsSpec extends Specification {

  "My solution should" should {

    "find the last element of a list. (P01)" in {
      last(List(1, 1, 2, 3, 5, 8)) must_== 8
    }

    "find the last but one element of a list. (P02)" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must_== 5
    }

    "find the Kth element of a list. (P03)" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) must_== 2
    }

  }

}
