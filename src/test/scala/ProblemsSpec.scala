import org.scalatest._

import com.kpiszczek.Problems

class ProblemsSpec extends FlatSpec with Matchers {
  "1: last" should "return None if list is empty" in {
    Problems.last[Any](Nil) should be (None)
  }
  it should "return last element of non-empty list" in {
  	Problems.last(1 :: 2 :: 3 :: Nil) should be (Some(3))
  	Problems.last(List(1, 1, 2, 3, 5, 8)) should be (Some(8))
  	Problems.last(1 :: Nil) should be (Some(1))
  }

  "2: penultimate" should "return None if list is empty" in {
    Problems.penultimate[Any](Nil) should be (None)
  }
  it should "return pre-last element of non-empty list" in {
  	Problems.penultimate(1 :: 2 :: 3 :: Nil) should be (Some(2))
  	Problems.penultimate(List(1, 1, 2, 3, 5, 8)) should be (Some(5))
  }
  it should "return None if list has only one element" in {
  	Problems.penultimate(1 :: Nil) should be (None)
  }

  "3: nth" should "return None if list is empty" in {
    Problems.penultimate[Any](Nil) should be (None)
  }
  it should "return nth element of non-empty list" in {
  	Problems.nth(0, 1 :: 2 :: 3 :: Nil) should be (Some(1))
  	Problems.nth(2, List(1, 1, 2, 3, 5, 8)) should be (Some(2))
  }
  it should "return None if list in shorter than n-elements" in {
  	Problems.nth(2, 1 :: Nil) should be (None)
  }

  "4: length" should "count number of elements in a list" in {
  	Problems.length[Any](Nil) should be (0)
  	Problems.length("hello" :: Nil) should be (1)
  	Problems.length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }

  "5: reverse" should "make a reversed list" in {
  	Problems.reverse[Any](Nil) should be (Nil)
  	Problems.reverse("hello" :: Nil) should be (List("hello"))
  	Problems.reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  "6: isPalindrome" should "check if given list is a palindrome" in {
  	Problems.isPalindrome[Any](Nil) should be (true)
  	Problems.isPalindrome("hello" :: Nil) should be (true)
  	Problems.isPalindrome(List(1, 1, 2, 3, 5, 8)) should be (false)
  	Problems.isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
  }

  "7: flatten" should "flatten a nested list structure" in {
  	Problems.flatten(Nil: List[Any]) should be (Nil)
  	Problems.flatten("hello" :: Nil) should be (List("hello"))
  	Problems.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }

  "8: compress" should "eliminate consecutive duplicates of list elements" in {
  	Problems.compress[Any](Nil) should be (Nil)
  	Problems.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  	Problems.compress(List("hello")) should be (List("hello"))
  	Problems.compress(List(1, 1, 2, 3, 5, 8)) should be (List(1, 2, 3, 5, 8))
  }

  "9: pack" should "pack consecutive duplicates of list elements into sublists" in {
  	Problems.pack[Any](Nil) should be (Nil)
  	Problems.pack(List("hello")) should be (List(List("hello")))
  	Problems.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "10: encode" should "run-length encoding of a list" in {
  	Problems.encode[Any](Nil) should be (Nil)
  	Problems.encode(List("hello")) should be (List((1, "hello")))
  	Problems.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "11: encodeModified" should "transfer only elements with duplicates as (N, E) single one should remain untouched" in {
  	Problems.encodeModified[Any](Nil) should be (Nil)
  	Problems.encodeModified(List("hello")) should be (List("hello"))
  	Problems.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  "12: decode" should "decode a run-length encoded list" in {
  	Problems.decode[Any](Nil) should be (Nil)
  	Problems.decode(List((1, "hello"))) should be (List("hello"))
  	Problems.decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "13: encodeDirect" should "compute run-length encoding of a list (direct solution)" in {
  	Problems.encodeDirect[Any](Nil) should be (Nil)
  	Problems.encodeDirect(List("hello")) should be (List((1, "hello")))
  	Problems.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}
