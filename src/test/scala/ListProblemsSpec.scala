import org.scalatest._
import org.scalatest.OptionValues._

import com.kpiszczek.ListProblems

class ListProblemsSpec extends FlatSpec with Matchers {
  import ListProblems._
  "1: last" should "return None if list is empty" in {
    last[Any](Nil) should be(None)
  }
  it should "return last element of non-empty list" in {
    last(1 :: 2 :: 3 :: Nil) should be(Some(3))
    last(List(1, 1, 2, 3, 5, 8)) should be(Some(8))
    last(1 :: Nil) should be(Some(1))
  }

  "2: penultimate" should "return None if list is empty" in {
    penultimate[Any](Nil) should be(None)
  }
  it should "return pre-last element of non-empty list" in {
    penultimate(1 :: 2 :: 3 :: Nil) should be(Some(2))
    penultimate(List(1, 1, 2, 3, 5, 8)) should be(Some(5))
  }
  it should "return None if list has only one element" in {
    penultimate(1 :: Nil) should be(None)
  }

  "3: nth" should "return None if list is empty" in {
    penultimate[Any](Nil) should be(None)
  }
  it should "return nth element of non-empty list" in {
    nth(0, 1 :: 2 :: 3 :: Nil) should be(Some(1))
    nth(2, List(1, 1, 2, 3, 5, 8)) should be(Some(2))
  }
  it should "return None if list in shorter than n-elements" in {
    nth(2, 1 :: Nil) should be(None)
  }

  "4: length" should "count number of elements in a list" in {
    ListProblems.length[Any](Nil) should be(0)
    ListProblems.length("hello" :: Nil) should be(1)
    ListProblems.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }

  "5: reverse" should "make a reversed list" in {
    reverse[Any](Nil) should be(Nil)
    reverse("hello" :: Nil) should be(List("hello"))
    reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  "6: isPalindrome" should "check if given list is a palindrome" in {
    isPalindrome[Any](Nil) should be(true)
    isPalindrome("hello" :: Nil) should be(true)
    isPalindrome(List(1, 1, 2, 3, 5, 8)) should be(false)
    isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }

  "7: flatten" should "flatten a nested list structure" in {
    flatten(Nil: List[Any]) should be(Nil)
    flatten("hello" :: Nil) should be(List("hello"))
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  "8: compress" should "eliminate consecutive duplicates of list elements" in {
    compress[Any](Nil) should be(Nil)
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
    compress(List("hello")) should be(List("hello"))
    compress(List(1, 1, 2, 3, 5, 8)) should be(List(1, 2, 3, 5, 8))
  }

  "9: pack" should "pack consecutive duplicates of list elements into sublists" in {
    pack[Any](Nil) should be(Nil)
    pack(List("hello")) should be(List(List("hello")))
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "10: encode" should "run-length encoding of a list" in {
    encode[Any](Nil) should be(Nil)
    encode(List("hello")) should be(List((1, "hello")))
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "11: encodeModified" should "transfer only elements with duplicates as (N, E) single one should remain untouched" in {
    encodeModified[Any](Nil) should be(Nil)
    encodeModified(List("hello")) should be(List("hello"))
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  "12: decode" should "decode a run-length encoded list" in {
    decode[Any](Nil) should be(Nil)
    decode(List((1, "hello"))) should be(List("hello"))
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "13: encodeDirect" should "compute run-length encoding of a list (direct solution)" in {
    encodeDirect[Any](Nil) should be(Nil)
    encodeDirect(List("hello")) should be(List((1, "hello")))
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "14: duplicate" should "duplicate the elements of a list" in {
    duplicate[Any](Nil) should be(Nil)
    duplicate(List("hello")) should be(List("hello", "hello"))
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "15: duplicateN" should "duplicate the elements of a list a given number of times" in {
    duplicateN[Any](153, Nil) should be(Nil)
    duplicateN(2, List("hello")) should be(List("hello", "hello"))
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "16: drop" should "drop every Nth element from a list" in {
    drop[Any](4, Nil) should be(Nil)
    drop(2, List("hello")) should be(List("hello"))
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "17: split" should "split list into two" in {
    split[Any](4, Nil) should be((Nil, Nil))
    split(2, List("hello")) should be((List("hello"), Nil))
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "18: slice" should "extract a slice from a list" in {
    slice[Any](2, 5, Nil) should be(Nil)
    slice(0, 3, List("hello")) should be(List("hello"))
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

  "19: rotate" should "rotate a list N places to the left" in {
    rotate[Any](2, Nil) should be(Nil)
    rotate(3, List("hello")) should be(List("hello"))
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "20: removeAt" should "Return the list and the removed element in a Tuple" in {
    removeAt[Any](1, Nil) should be((Nil, None))
    removeAt(0, List("hello")) should be((Nil, Some("hello")))
    removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), Some('b)))
  }

  "21: insertAt" should "insert an element at a given position into a list" in {
    insertAt(1, 1, Nil) should be(Nil)
    insertAt("hi", 0, List("hello")) should be(List("hi", "hello"))
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

  "22: range" should "create a list containing all integers within a given range" in {
    range(1, -1) should be(Nil)
    range(1, 1) should be(List(1))
    range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
  }

  "23: randomSelect" should "Extract a given number of randomly selected elements from a list" in {
  	randomSelect[Any](3, Nil) should be (None)
  	randomSelect(3, List("hello")) should be (None)
  	val res = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  	res.value.length should be (3)
  }

  "24: lotto" should "Draw N different random numbers from the set 1..M" in {
  	val res = lotto(6, 49)
  	res.value.length should be (6)
  }

  "25: randomPermute" should "generate a random permutation of the elements of a list" in {
  	val input = List('a, 'b, 'c, 'd, 'e, 'f)
  	val result = randomPermute(input)
  	result should contain theSameElementsAs (input)
  }

  "26: combinations" should "generate all n-element combinations of given list" in {
    val l = combinations(4, List('a, 'b, 'c, 'd, 'e, 'f))
    l.length should be (20)
  }

  "28a: lsort" should "sort a list of lists according to length of sublists" in {
    val l = lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    l should be (List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }

  "28b: lsortFreq" should "sort the elements according to their length frequency" in {
    val l = lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    l should be (List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  }
}
