package com.kpiszczek

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object ListProblems {
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

  def encodeModified[A : ClassTag](as: List[A]): List[_] =
    pack(as) collect {case a: List[A] => length(a) match {
      case len if len > 1 => (len, a.head)
      case 1 => a.head
    }}

  def decode[A : ClassTag](as: List[(Int, A)]): List[A] =
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

  def drop[A : ClassTag](n: Int, as: List[A]): List[A] =
    List(as.zipWithIndex collect ({
      case (a: A, i: Int) if (i + 1) % 3 != 0 => a
    }): _*)

  def split[A](n: Int, as: List[A]): (List[A], List[A]) = {
    def reducer(a: (A, Int), acc: (List[A], List[A])) =
      if (a._2 >= n) (acc._1, a._1 :: acc._2)
      else (a._1 :: acc._1, acc._2)

    as.zipWithIndex.foldRight[(List[A], List[A])]((Nil, Nil))(reducer)
  }

  def slice[A : ClassTag](n: Int, k: Int, as: List[A]): List[A] =
    List(as.zipWithIndex collect ({
      case (a: A, i: Int) if (i >= n && i < k) => a
    }):_*)

  def rotate[A](n: Int, as: List[A]): List[A] = {
    def revConcat(a: (List[A], List[A])) = a._2 ::: a._1
    revConcat(if (n >= 0) as.splitAt(n) else as.splitAt(as.length + n))
  }

  def removeAt[A](n: Int, as: List[A]): (List[A], Option[A]) = {
    def reducer(a: (A, Int), acc: (List[A], Option[A])) = {
      val idx = a._2
      val elem = a._1
      if (idx == n) (acc._1, Some(elem))
      else (elem :: acc._1, acc._2)
    }
    as.zipWithIndex.foldRight[(List[A], Option[A])]((Nil, None))(reducer)
  }

  def insertAt[A](a: A, k: Int, as: List[A]): List[A] = {
    def reducer(elem: (A, Int), acc: List[A]) =
      if (elem._2 == k) a :: elem._1 :: acc
      else elem._1 :: acc
    as.zipWithIndex.foldRight[List[A]](Nil)(reducer)
  }

  def range(start: Int, end: Int): List[Int] = {
    @tailrec
    def go(idx: Int, acc: List[Int]): List[Int] =
      if (idx == start) idx :: acc
      else go(idx - 1, idx :: acc)
    if (end < start) Nil
    else go(end, Nil)
  }

  def randomSelect[A](n: Int, as: List[A]): Option[List[A]] = {
    val random = new Random(System.currentTimeMillis)
    val length = as.length
    def go(acc: List[A], as: List[A], i: Int): List[A] = {
      if (i == n) acc
      else {
        val (tail, a) = removeAt[A](random.nextInt(length - i), as)
        // cannot be None
        go(a.get :: acc, tail, i + 1)
      }
    }
    if (n > length) None
    else Some(go(Nil, as, 0))
  }

  def lotto(n: Int, k: Int): Option[List[Int]] = randomSelect(n, range(1, k))

  def randomPermute[A](as: List[A]): List[A] = randomSelect(as.length, as).get

  def combinations[A](n: Int, as: List[A]): List[List[A]] = (n, as) match {
    case (_, Nil) ⇒ Nil
    case (0, _) ⇒ List(Nil)
    case (n, h :: t) ⇒ (combinations(n - 1, t) map (h :: _)) ::: combinations(n, t)
  }

  def lsort[A](as: List[List[A]]): List[List[A]] =
    as sortBy (_.length)

  def lsortFreq[A](as: List[List[A]]): List[List[A]] =
    as.groupBy(_.length).toList sortBy (_._2.length) map (_._2) flatten
}