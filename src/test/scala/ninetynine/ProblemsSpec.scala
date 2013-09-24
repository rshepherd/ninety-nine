package ninetynine

import Problems._
import org.specs2.mutable._

class ProblemsSpec extends Specification {

  "Take one down, pass it around.." should {

    "find the last element of a list. (P01)" in {
      last(List(1, 1, 2, 3, 5, 8)) must_== Some(8)
    }

    "find the last but one element of a list. (P02)" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must_== Some(5)
    }

    "find the Kth element of a list. (P03)" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) must_== Some(2)
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
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }

    "get modified run-length encoding. (P11)" in {
      encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    }

    "decode a run-length encoded list. (P12)" in {
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) must_==
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }

    "run-length encoding of a list (direct solution). (P13)" in {
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }

    "duplicate the elements of a list. (P14)" in {
      duplicate(List('a, 'b, 'c, 'c, 'd)) must_==
        List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    }

    "duplicate the elements of a list a given number of times. (P15)" in {
      duplicate(List('a, 'b, 'c, 'c, 'd), 3) must_==
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    }

    "drop every Nth element from a list. (P16)" in {
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }

    "split a list into two parts. (P17)" in {
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

    "extract a slice from a list. (P18) " in {
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('d, 'e, 'f, 'g)
    }

    "rotate a list N places to the left (P19)" in {
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
                List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
                 List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    }

    "remove the Kth element from a list (P20)" in {
      removeAt(1, List('a, 'b, 'c, 'd)) must_== (List('a, 'c, 'd),'b)
    }

    "insert an element at a given position into a list. (P21)" in {
      insertAt('new, 1, List('a, 'b, 'c, 'd)) must_== List('a, 'new, 'b, 'c, 'd)
    }

    "create a list containing all integers within a given range. (P22)" in {
      range(4, 9) must_== List(4, 5, 6, 7, 8, 9)
    }

    "extract a given number of randomly selected elements from a list. (P23)" in {
      randomSelect(3, List(4, 5, 6, 7, 8, 9)).distinct.size must_== 3
      randomSelect(3, List(4, 5, 6, 7, 8, 9)) forall {
        List(4, 5, 6, 7, 8, 9) contains _
      }
    }

    "draw N different random numbers from the set 1..M (P24)" in {
      lotto(3, 9).distinct.size must_== 3
      lotto(3, 9) forall { _ <= 9 }
      lotto(99, 99) forall { _ <= 99 }
    }

    "generate a random permutation of the elements of a list. (P25)" in {
      val l = List(1 ,2, 3, 4, 5)
      val p = permute(l)
      p.size must_== 5
      p forall { l contains _ }
    }

  }

}
