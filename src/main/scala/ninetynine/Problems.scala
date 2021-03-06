package ninetynine

object Problems {

  // P01 (*) Find the last element of a list.
  def last[A](l: List[A]): Option[A] = l match {
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
    case _ => None
  }

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](l: List[A]): Option[A] = l match {
    case x :: _ :: Nil => Some(x)
    case x :: xs => penultimate(xs)
    case _ => None
  }

  // P03 (*) Find the Nth element of a list.
  def elementAt[A](i: Int, list: List[A]): Option[A] = {
    if(i >= list.size) None
    else Some(list.zipWithIndex.reduceLeft((a, b) => if(b._2 == i) b else a )._1)
  }

  def elementAtRecursive[A](i: Int, l: List[A]): Option[A] = l match {
    case x :: xs if xs.size == l.size - i - 1 => Some(x)
    case x :: xs => elementAtRecursive(i - 1, xs)
    case _ => None
  }

  // P04 (*) Find the number of elements of a list.
  def len[A](l: List[A]): Int = {
    def length[B](a: Int, l: List[B]): Int = l match {
      case Nil => 0
      case x :: Nil => a + 1
      case x :: xs => length(a + 1, xs)
    }
    length(0, l)
  }

  // P05 (*) Reverse a list.
  def rev[A](list: List[A]) = list.foldLeft(List[A]()) { (l, a) => a :: l }

  def revRecursive[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => revRecursive(xs) ::: List(x)
  }

  // P06 (*) Find out whether a list is a palindrome.
  def palindrome[A](list: List[A]) = {
    def compare(l1: List[A], l2: List[A]): Boolean = {
      if (l1.isEmpty && l2.isEmpty) true
      else if (l1.head != l2.head) false
      else compare(l1.tail, l2.tail)
    }
    compare(list, list.reverse)
  }

  // P07 (**) Flatten a nested list structure.
  def flatten(lists: List[Any]) : List[Any] = lists.flatMap {
    case l: List[Any] => flatten(l)
    case a: Any => Some(a)
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
  def encode[A](list: List[A]): List[(Int, A)] =
    pack(list).foldLeft(List[(Int, A)]()) {
      (ll, l) => (l.size, l.head) :: ll
    }.reverse

  // P11 (*) Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[Any] =
    encode(list).foldLeft(List[Any]()) {
      (l, t) => if(t._1 == 1) t._2 :: l else t :: l
    }.reverse

  // P12 (**) Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]) = list.flatMap { t => 1 to t._1 map { _ => t._2 } }

  // P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect[A](list: List[A]): List[(Int, A)] = encode(list) // TODO

  // P14 && P15 (**) Duplicate the elements of a list.
  def duplicate[A](list: List[A], n: Int = 2) = list.flatMap { List.fill(n)(_) }

  // P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A]) = {
    list.zipWithIndex.filter { t => (t._2 + 1) % n != 0 } map (_._1)
  }

  // P17 (*) Split a list into two parts. i.e. list.splitAt(i)
  def split[A](i: Int, list: List[A]) = {
    val (l1, l2) = list.zipWithIndex.foldLeft((List[A](), List[A]())) { (acc, e) =>
      if (e._2 < i) (e._1 :: acc._1, acc._2) else (acc._1, e._1 :: acc._2)
    }
    (l1.reverse, l2.reverse)
  }

  // P18 (**) Extract a slice from a list. i.e. list.slice(s, e)
  def slice[A](s: Int, e: Int, list: List[A]) = {
    list.zipWithIndex.filter { t => t._2 >= s && t._2 < e } map { _._1 }
  }

  // P19 (**) Rotate a list N places to the left.
  def rotate[A](i: Int, list: List[A]): List[A] = {
    val (h, t) = split(if (i < 0) list.size + i else i, list)
    t ++ h
  }

  // P20 (*) Remove the Kth element from a list.
  def removeAt[A](i: Int, list: List[A]): (List[A], A) = {
    val (h, t) = split(i, list)
    (h ++ t.drop(1), t.head)
  }

  // P21 (*) Insert an element at a given position into a list.
  def insertAt[A](x: A, i: Int, list: List[A]): List[A] =
    list.zipWithIndex.foldLeft(List[A]()) { (acc, e) =>
      if(e._2 == i) e._1 :: x :: acc else e._1 :: acc
    }.reverse

  // P22 (*) Create a list containing all integers within a given range.
  def range(s: Int, e: Int): List[Int] = if (s > e) Nil else s :: range(s + 1, e)

  // P23 (**) Extract a given number of randomly selected elements from a list
  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    if(n == 0) Nil
    else {
      val (l, e) = removeAt(scala.util.Random.nextInt(list.size), list)
      e :: randomSelect(n - 1, l)
    }
  }

  // P24 (*) Lotto: Draw N different random numbers from the set 1..M
  def lotto(n: Int, max: Int): List[Int] = randomSelect(n, range(1, max))

  // P25 (*) Generate a random permutation of the elements of a list.
  def permutation[A](l: List[A]): List[A] = randomSelect(l.length, l)

  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[A](n: Int, list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case _ if n == 1 => list.map(List(_))
    case x :: xs => combinations(n - 1, xs).map(x :: _) ++ combinations(n, xs)
  }

  // P31 (**) (Very naive way to) Determine whether a given integer number is prime.
  implicit def _prime(i: Int): Prime = Prime(i)
  case class Prime(i: Int) {
    def isPrime: Boolean = {
      def isPrime(l: Int, mod: Int) : Boolean = {
        if(mod == 1) true
        else if(l % mod == 0) false
        else isPrime(l, mod - 1)
      }
      if (i < 2 || (i > 2 && i % 2 == 0)) false else isPrime(i, Math.sqrt(i).toInt)
    }
  }

  // P32 (**) Determine the greatest common divisor of two positive integer numbers.
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // P33 (*) Determine whether two positive integer numbers are coprime.
  implicit def _coprime(i: Int): Coprime = Coprime(i)
  case class Coprime(a: Int) {
    def isCoprime(b: Int): Boolean = gcd(a, b) == 1
  }

  // P34 (**) Calculate Euler's totient function
  implicit def _totient(i: Int): Totient = Totient(i)
  case class Totient(a: Int) {
    def φ = (1 to a).count { i =>
      i isCoprime a
    }
  }

  // P35 (**) Determine the prime factors of a given positive integer.
  implicit def _primeFactors(i: Int): PrimeFactors = PrimeFactors(i)
  case class PrimeFactors(i: Int) {
    def primeFactors: List[Int] = {
      def factorize(i: Int, f: Int = 1, factors: List[Int] = Nil): List[Int] = {
        if (f > i)
          factors
        else if (i % f == 0 && f.isPrime)
          factorize(i / f, f, f :: factors)
        else
          factorize(i, f + 1, factors)
      }
      factorize(i).sorted
    }
  }

  // P36 (**) Determine the prime factors of a given positive integer (2).
  def primeFactorMultiplicity(i : Int): List[(Int, Int)] = encode(i.primeFactors)

  // P39 (*) Extract prime numbers from a range.
  def listPrimesInRange(r: Range): IndexedSeq[Int] = r.filter(_.isPrime)

  // P40 (**) Goldbach conjecture.
  implicit def _goldbach(i: Int): Goldbach = Goldbach(i)
  case class Goldbach(i: Int) {
    def goldbach = listPrimesInRange(2 to i).foldLeft((0, 0)) { (x, y) =>
      if((i - y).isPrime) (i - y, y) else x
    }
  }

}
