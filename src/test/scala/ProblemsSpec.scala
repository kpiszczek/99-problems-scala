import org.scalatest._

import com.kpiszczek.Problems

class ProblemsSpec extends FlatSpec with Matchers {
  "1: last" should "return None if list is empty" in {
    Problems.last[Any](Nil) should be(None)
  }
  it should "return last element of non-empty list" in {
    Problems.last(1 :: 2 :: 3 :: Nil) should be(Some(3))
    Problems.last(List(1, 1, 2, 3, 5, 8)) should be(Some(8))
    Problems.last(1 :: Nil) should be(Some(1))
  }

  "2: penultimate" should "return None if list is empty" in {
    Problems.penultimate[Any](Nil) should be(None)
  }
  it should "return pre-last element of non-empty list" in {
    Problems.penultimate(1 :: 2 :: 3 :: Nil) should be(Some(2))
    Problems.penultimate(List(1, 1, 2, 3, 5, 8)) should be(Some(5))
  }
  it should "return None if list has only one element" in {
    Problems.penultimate(1 :: Nil) should be(None)
  }

  "3: nth" should "return None if list is empty" in {
    Problems.penultimate[Any](Nil) should be(None)
  }
  it should "return nth element of non-empty list" in {
    Problems.nth(0, 1 :: 2 :: 3 :: Nil) should be(Some(1))
    Problems.nth(2, List(1, 1, 2, 3, 5, 8)) should be(Some(2))
  }
  it should "return None if list in shorter than n-elements" in {
    Problems.nth(2, 1 :: Nil) should be(None)
  }

  "4: length" should "count number of elements in a list" in {
    Problems.length[Any](Nil) should be(0)
    Problems.length("hello" :: Nil) should be(1)
    Problems.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }

  "5: reverse" should "make a reversed list" in {
    Problems.reverse[Any](Nil) should be(Nil)
    Problems.reverse("hello" :: Nil) should be(List("hello"))
    Problems.reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  "6: isPalindrome" should "check if given list is a palindrome" in {
    Problems.isPalindrome[Any](Nil) should be(true)
    Problems.isPalindrome("hello" :: Nil) should be(true)
    Problems.isPalindrome(List(1, 1, 2, 3, 5, 8)) should be(false)
    Problems.isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }

  "7: flatten" should "flatten a nested list structure" in {
    Problems.flatten(Nil: List[Any]) should be(Nil)
    Problems.flatten("hello" :: Nil) should be(List("hello"))
    Problems.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  "8: compress" should "eliminate consecutive duplicates of list elements" in {
    Problems.compress[Any](Nil) should be(Nil)
    Problems.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
    Problems.compress(List("hello")) should be(List("hello"))
    Problems.compress(List(1, 1, 2, 3, 5, 8)) should be(List(1, 2, 3, 5, 8))
  }

  "9: pack" should "pack consecutive duplicates of list elements into sublists" in {
    Problems.pack[Any](Nil) should be(Nil)
    Problems.pack(List("hello")) should be(List(List("hello")))
    Problems.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "10: encode" should "run-length encoding of a list" in {
    Problems.encode[Any](Nil) should be(Nil)
    Problems.encode(List("hello")) should be(List((1, "hello")))
    Problems.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "11: encodeModified" should "transfer only elements with duplicates as (N, E) single one should remain untouched" in {
    Problems.encodeModified[Any](Nil) should be(Nil)
    Problems.encodeModified(List("hello")) should be(List("hello"))
    Problems.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  "12: decode" should "decode a run-length encoded list" in {
    Problems.decode[Any](Nil) should be(Nil)
    Problems.decode(List((1, "hello"))) should be(List("hello"))
    Problems.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "13: encodeDirect" should "compute run-length encoding of a list (direct solution)" in {
    Problems.encodeDirect[Any](Nil) should be(Nil)
    Problems.encodeDirect(List("hello")) should be(List((1, "hello")))
    Problems.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "14: duplicate" should "duplicate the elements of a list" in {
    Problems.duplicate[Any](Nil) should be(Nil)
    Problems.duplicate(List("hello")) should be(List("hello", "hello"))
    Problems.duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "15: duplicateN" should "duplicate the elements of a list a given number of times" in {
    Problems.duplicateN[Any](153, Nil) should be(Nil)
    Problems.duplicateN(2, List("hello")) should be(List("hello", "hello"))
    Problems.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "16: drop" should "drop every Nth element from a list" in {
    Problems.drop[Any](4, Nil) should be(Nil)
    Problems.drop(2, List("hello")) should be(List("hello"))
    Problems.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "17: split" should "split list into two" in {
    Problems.split[Any](4, Nil) should be((Nil, Nil))
    Problems.split(2, List("hello")) should be((List("hello"), Nil))
    Problems.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "18: slice" should "extract a slice from a list" in {
    Problems.slice[Any](2, 5, Nil) should be(Nil)
    Problems.slice(0, 3, List("hello")) should be(List("hello"))
    Problems.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

  "19: rotate" should "rotate a list N places to the left" in {
    Problems.rotate[Any](2, Nil) should be(Nil)
    Problems.rotate(3, List("hello")) should be(List("hello"))
    Problems.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    Problems.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "20: removeAt" should "Return the list and the removed element in a Tuple" in {
    Problems.removeAt[Any](1, Nil) should be((Nil, None))
    Problems.removeAt(0, List("hello")) should be((Nil, Some("hello")))
    Problems.removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), Some('b)))
  }

  "21: insertAt" should "insert an element at a given position into a list" in {
 	Problems.insertAt(1, 1, Nil) should be(Nil)
 	Problems.insertAt("hi", 0, List("hello")) should be(List("hi", "hello"))
 	Problems.insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

 "22: range" should "create a list containing all integers within a given range" in {
   Problems.range(1, -1) should be (Nil)
   Problems.range(1, 1) should be(List(1))
   Problems.range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
 }
}
