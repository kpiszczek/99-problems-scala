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

  def flatten(as: List[_]) : List[_] = as flatMap {
    case a : List[_] => flatten(a)
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
}
