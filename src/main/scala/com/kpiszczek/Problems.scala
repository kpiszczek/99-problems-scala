package com.kpiszczek

import scala.annotation.tailrec

object Problems {
  @tailrec
  def last[A](as: List[A]): Option[A] = as match {
    case Nil => None
    case a :: Nil => Some(a)
    case _ :: tail => last(tail)
  }

  @tailrec
  def penultimate[A](as: List[A]): Option[A] = as match {
    case Nil | _ :: Nil => None
    case a :: _ :: Nil => Some(a)
    case _ :: tail => penultimate(tail)
  }

  @tailrec
  def nth[A](n: Int, as: List[A]): Option[A] = (n, as) match {
    case (_, Nil) => None
    case (0, a :: _) => Some(a)
    case (n, _ :: tail) => nth(n - 1, tail)
  }

  def length[A](as: List[A]): Int =
    as.foldLeft(0)((acc: Int, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    as.foldLeft(Nil: List[A])((acc: List[A], a: A) => a :: acc)

  def isPalindrome[A](as: List[A]): Boolean =
    as == reverse(as)

  def flatten(as: List[_]): List[_] = as flatMap {
    case a: List[_] => flatten(a)
    case a => List(a)
  }

  def compress[A](as: List[A]): List[A] = {
    @tailrec
    def go(acc: List[A], last: A, as: List[A]): List[A] = as match {
      case Nil => last :: acc
      case a :: rest =>
        if (a == last) go(acc, last, rest)
        else go(last :: acc, a, rest)
    }
    as match {
      case Nil => Nil
      case a :: rest => reverse(go(Nil, a, rest))
    }
  }

  def pack[A](as: List[A]): List[List[A]] = {
    @tailrec
    def go(acc: List[List[A]], last: List[A], as: List[A]): List[List[A]] = as match {
      case Nil => last :: acc
      case a :: rest =>
        if (a == last.head) go(acc, a :: last, rest)
        else go(last :: acc, List(a), rest)
    }
    as match {
      case Nil => Nil
      case a :: rest => reverse(go(Nil, List(a), rest))
    }
  }

  def encode[A](as: List[A]): List[(Int, A)] =
    pack(as) flatMap ((a: List[A]) => List((length(a), a.head)))

  def encodeModified[A](as: List[A]): List[_] =
    pack(as) collect ((a: List[A]) => length(a) match {
      case len if len > 1 => (len, a.head)
      case 1 => a.head
    })

  def decode[A](as: List[(Int, A)]): List[A] =
    as flatMap ({
      case (len: Int, a: A) => List.fill(len)(a)
    })

  def encodeDirect[A](as: List[A]): List[(Int, A)] = {
  	@tailrec
  	def go(acc: List[(Int, A)], last: (Int, A), as: List[A]): List[(Int, A)] = as match {
  	  case Nil => last :: acc
  	  case a :: rest =>
  	    if (a == last._2) go(acc, (last._1 + 1, a), rest)
  	    else go(last :: acc, (1, a), rest)
  	}
  	as match {
  	  case Nil => Nil
  	  case a :: rest => reverse(go(Nil, (1, a), rest))
  	}
  }

  def duplicate[A](as: List[A]): List[A] = as flatMap ((a: A) => List(a, a))

  def duplicateN[A](n: Int, as: List[A]): List[A] = as flatMap ((a: A) => List.fill(n)(a))

  def drop[A](n: Int, as: List[A]): List[A] = 
    as.zipWithIndex collect ({
    	case (a: A, i: Int) if (i + 1) % 3 != 0 => a
    })

  def split[A](n: Int, as: List[A]): (List[A], List[A]) = {
    def reducer(acc: (List[A], List[A]), a: (A, Int)) = 
      if (a._2 >= n) (acc._1, a._1 :: acc._2)
	  else (a._1 :: acc._1, acc._2)

    val res = as.zipWithIndex.foldLeft[(List[A], List[A])]((Nil, Nil))(reducer)

    (reverse(res._1), reverse(res._2))
  }
}	
