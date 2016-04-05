// Chapter 3
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else chapters.Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalStateException("Can not get tail of nil!")
    case Cons(_, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalStateException("Can not set head on nil!")
    case Cons(_, xs) => chapters.Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case _ => l
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalStateException("Can not take init on nil!")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => chapters.Cons(x, init(xs))
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight2(xs, z)(f))
  }

  // Exercise 3.9
  // Compute the length of a list using foldRight
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, r) => 1 + r)

  // Exercise 3.10
  // implement a tail recursive foldLeft
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sum2(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product2(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  // Exercise 3.12
  // implement reverse in terms of a fold
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((b, a) => chapters.Cons(a, b))

  // Exercise 3.13
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  // Exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(chapters.Cons(_, _))

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // Exercise 3.16
  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => chapters.Cons(a + 1, b))

  // Exercise 3.17
  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => chapters.Cons(a.toString, b))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => chapters.Cons(f(a), b))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) chapters.Cons(a, b) else b)

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, b) => append(f(a), b))

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22
  def addLists(l: List[Int], r: List[Int]): List[Int] =
    (l,r) match {
      case (Cons(x,xs), Cons(y, ys)) => chapters.Cons(x + y, addLists(xs, ys))
      case _ => Nil
    }

  // Exercise 3.23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def go(l: List[A], r: List[B], acc: List[C]): List[C] =
      (l, r) match {
        case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, chapters.Cons(f(x, y), acc))
        case _ => reverse(acc) // must reverse since we are building in reverse order
      }
      go(l, r, Nil:List[C])
  }

  // Exercise 3.24
  def hasSubsequence[A,B](l: List[A], s: List[B]): Boolean = {

    def startsWith(l: List[A], s: List[B]): Boolean = (l, s) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
      case _ => false
    }

    if (startsWith(l, s)) true
    else l match {
      case Nil => false
      case Cons(_, xs) => hasSubsequence(xs, s)
    }

  }


}

import List._
drop(List(2,3,4), 1)
dropWhile(List(2,4,6,1,2,3,4), (a: Int) => a % 2 == 0)
init(List(1,2,3))
length(List(1,3,4))
reverse(List(1,2,3))
foldRight(List(1,2,3,4), Nil:List[Int])(chapters.Cons(_, _))
foldLeft2(List(1,2,3,4), Nil:List[Int])((a,b) => chapters.Cons(b, a))
append(List(1,2,3), List(4,5,6))
concat(List(List(1,2), List(3,4), List(5,6)))
increment(List(1,2,3))
double2String(List(1.0, 2.0, 3.0))
map(List(1,2,3))(_ + 3)
filter(List(1,2,3,4,5))(_ % 2 == 0)
flatMap(List(1,2,3))(i => List(i, i))
addLists(List(1,2,3), List(2,3,4))
zipWith(List(1,2,3), List(2,3,4))(_ + _)
hasSubsequence(List(1,2,3,4), List(2,3))