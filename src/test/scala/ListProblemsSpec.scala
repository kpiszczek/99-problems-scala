import org.scalatest._
import org.scalatest.OptionValues._

import com.kpiszczek.ListProblems

class ListProblemsSpec extends FlatSpec with Matchers {
  "1: last" should "return None if list is empty" in {
    ListProblems.last[Any](Nil) should be(None)
  }
  it should "return last element of non-empty list" in {
    ListProblems.last(1 :: 2 :: 3 :: Nil) should be(Some(3))
    ListProblems.last(List(1, 1, 2, 3, 5, 8)) should be(Some(8))
    ListProblems.last(1 :: Nil) should be(Some(1))
  }

  "2: penultimate" should "return None if list is empty" in {
    ListProblems.penultimate[Any](Nil) should be(None)
  }
  it should "return pre-last element of non-empty list" in {
    ListProblems.penultimate(1 :: 2 :: 3 :: Nil) should be(Some(2))
    ListProblems.penultimate(List(1, 1, 2, 3, 5, 8)) should be(Some(5))
  }
  it should "return None if list has only one element" in {
    ListProblems.penultimate(1 :: Nil) should be(None)
  }

  "3: nth" should "return None if list is empty" in {
    ListProblems.penultimate[Any](Nil) should be(None)
  }
  it should "return nth element of non-empty list" in {
    ListProblems.nth(0, 1 :: 2 :: 3 :: Nil) should be(Some(1))
    ListProblems.nth(2, List(1, 1, 2, 3, 5, 8)) should be(Some(2))
  }
  it should "return None if list in shorter than n-elements" in {
    ListProblems.nth(2, 1 :: Nil) should be(None)
  }

  "4: length" should "count number of elements in a list" in {
    ListProblems.length[Any](Nil) should be(0)
    ListProblems.length("hello" :: Nil) should be(1)
    ListProblems.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }

  "5: reverse" should "make a reversed list" in {
    ListProblems.reverse[Any](Nil) should be(Nil)
    ListProblems.reverse("hello" :: Nil) should be(List("hello"))
    ListProblems.reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  "6: isPalindrome" should "check if given list is a palindrome" in {
    ListProblems.isPalindrome[Any](Nil) should be(true)
    ListProblems.isPalindrome("hello" :: Nil) should be(true)
    ListProblems.isPalindrome(List(1, 1, 2, 3, 5, 8)) should be(false)
    ListProblems.isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }

  "7: flatten" should "flatten a nested list structure" in {
    ListProblems.flatten(Nil: List[Any]) should be(Nil)
    ListProblems.flatten("hello" :: Nil) should be(List("hello"))
    ListProblems.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  "8: compress" should "eliminate consecutive duplicates of list elements" in {
    ListProblems.compress[Any](Nil) should be(Nil)
    ListProblems.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
    ListProblems.compress(List("hello")) should be(List("hello"))
    ListProblems.compress(List(1, 1, 2, 3, 5, 8)) should be(List(1, 2, 3, 5, 8))
  }

  "9: pack" should "pack consecutive duplicates of list elements into sublists" in {
    ListProblems.pack[Any](Nil) should be(Nil)
    ListProblems.pack(List("hello")) should be(List(List("hello")))
    ListProblems.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "10: encode" should "run-length encoding of a list" in {
    ListProblems.encode[Any](Nil) should be(Nil)
    ListProblems.encode(List("hello")) should be(List((1, "hello")))
    ListProblems.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "11: encodeModified" should "transfer only elements with duplicates as (N, E) single one should remain untouched" in {
    ListProblems.encodeModified[Any](Nil) should be(Nil)
    ListProblems.encodeModified(List("hello")) should be(List("hello"))
    ListProblems.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  "12: decode" should "decode a run-length encoded list" in {
    ListProblems.decode[Any](Nil) should be(Nil)
    ListProblems.decode(List((1, "hello"))) should be(List("hello"))
    ListProblems.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "13: encodeDirect" should "compute run-length encoding of a list (direct solution)" in {
    ListProblems.encodeDirect[Any](Nil) should be(Nil)
    ListProblems.encodeDirect(List("hello")) should be(List((1, "hello")))
    ListProblems.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "14: duplicate" should "duplicate the elements of a list" in {
    ListProblems.duplicate[Any](Nil) should be(Nil)
    ListProblems.duplicate(List("hello")) should be(List("hello", "hello"))
    ListProblems.duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "15: duplicateN" should "duplicate the elements of a list a given number of times" in {
    ListProblems.duplicateN[Any](153, Nil) should be(Nil)
    ListProblems.duplicateN(2, List("hello")) should be(List("hello", "hello"))
    ListProblems.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "16: drop" should "drop every Nth element from a list" in {
    ListProblems.drop[Any](4, Nil) should be(Nil)
    ListProblems.drop(2, List("hello")) should be(List("hello"))
    ListProblems.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "17: split" should "split list into two" in {
    ListProblems.split[Any](4, Nil) should be((Nil, Nil))
    ListProblems.split(2, List("hello")) should be((List("hello"), Nil))
    ListProblems.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "18: slice" should "extract a slice from a list" in {
    ListProblems.slice[Any](2, 5, Nil) should be(Nil)
    ListProblems.slice(0, 3, List("hello")) should be(List("hello"))
    ListProblems.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

  "19: rotate" should "rotate a list N places to the left" in {
    ListProblems.rotate[Any](2, Nil) should be(Nil)
    ListProblems.rotate(3, List("hello")) should be(List("hello"))
    ListProblems.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    ListProblems.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "20: removeAt" should "Return the list and the removed element in a Tuple" in {
    ListProblems.removeAt[Any](1, Nil) should be((Nil, None))
    ListProblems.removeAt(0, List("hello")) should be((Nil, Some("hello")))
    ListProblems.removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), Some('b)))
  }

  "21: insertAt" should "insert an element at a given position into a list" in {
    ListProblems.insertAt(1, 1, Nil) should be(Nil)
    ListProblems.insertAt("hi", 0, List("hello")) should be(List("hi", "hello"))
    ListProblems.insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

  "22: range" should "create a list containing all integers within a given range" in {
    ListProblems.range(1, -1) should be(Nil)
    ListProblems.range(1, 1) should be(List(1))
    ListProblems.range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
  }

  "23: randomSelect" should "Extract a given number of randomly selected elements from a list" in {
  	ListProblems.randomSelect[Any](3, Nil) should be (None)
  	ListProblems.randomSelect(3, List("hello")) should be (None)
  	val res = ListProblems.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  	res.value.length should be (3)
  }
}
