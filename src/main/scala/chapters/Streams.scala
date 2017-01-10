package chapters

/**
  * Created on 2016-03-30.
  */

sealed trait FStream[+A] {

  import FStream._

  // Exercise 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def go(acc: List[A], l: FStream[A]): List[A] =
      l match {
        case Empty => acc.reverse
        case Cons(h, t) => go(h() :: acc, t())
      }
    go(Nil, this)
  }

  // Exercise 5.2
  def take(n: Int): FStream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): FStream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile2(p: A => Boolean): FStream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile2(p))
    case _ => empty
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false) { (a, b) => p(a) || b }

  // Exercise 5.4
  def forall(p: A => Boolean): Boolean = {
    !this.exists((a: A) => !p(a))
  }

  def forall2(p: A => Boolean): Boolean =
    foldRight(true) {
      (a, b) => p(a) && b
    }

  // Exercise 5.5
  def takeWhile(p: A => Boolean): FStream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else empty
    }

  // Exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A]) {
      (a, _) => Some(a)
    }

  def map[B](f: A => B): FStream[B] =
    foldRight(empty[B]) {
      (a, b) => cons(f(a), b)
    }

  def filter(p: A => Boolean): FStream[A] =
    foldRight(empty[A]) {
      (a, b) => if (p(a)) cons(a, b) else b
    }

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def append[B >: A](l: => FStream[B]): FStream[B] =
    foldRight(l) {
      (a, b) => cons(a, b)
    }

  def flatMap[B](f: A => FStream[B]): FStream[B] =
    foldRight(empty[B]) {
      (a, b) => f(a) append b
    }

  // Exercise 5.13
  def map2[B](f: A => B): FStream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take2(n: Int): FStream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): FStream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](l: FStream[B])(f: (A, B) => C): FStream[C] =
    unfold((this, l)) {
      case (Cons(h, t), Cons(bh, bt)) => Some(f(h(), bh()), (t(), bt()))
      case _ => None
    }

  def zipAll[B](l: FStream[B]): FStream[(Option[A], Option[B])] =
    unfold((this, l)) {
      case (Cons(h, t), Cons(bh, bt)) => Some((Some(h()), Some(bh())), (t(), bt()))
      case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty[B]))
      case (_, Cons(bh, bt)) => Some((None, Some(bh())), (empty[A], bt()))
      case _ => None
    }

  def zip[B](l: FStream[B]): FStream[(A, B)] =
    zipWith(l)((a,b) => (a, b))

  // Exercise 5.14
  def startsWith[B](s: FStream[B]): Boolean =
    zipAll(s).foldRight(true) {
      (a, b) => a match {
        case (Some(a1), Some(a2)) if a1 == a2 => b
        case (Some(a1), None) => true
        case _ => false
      }
    }

  def startsWith2[B](s: FStream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isEmpty).forall({
      case (a1, b1) => a1 == b1
    })

  // Exercise 5.15
  def tails: FStream[FStream[A]] =
    unfold(this) {
      case Cons(h, t) if t() == Empty => Some(empty[A], t())
      case l@Cons(h, t) => Some(l, t())
      case _ => None
    }

  def hasSubsequence[A](s: FStream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  // Implement a function like fold which returns intermediate results
  // as a stream
  def scanRight[B](z: B)(f: (A, => B) => B): FStream[B] =
  // The zero case would be a stream with the zero element
    foldRight(cons(z, empty[B])) {
      // note: we can safely call b.headOption.get since b can never be empty
      // we start b off as a non empty stream and keep prepending to it
      // we also retain the lazy-ness of the list since f's second arg is by-name
      // so the b.headOption.get may not even get called
      (a, b) => cons(f(a, b.headOption.get), b)
    }
}

case object Empty extends FStream[Nothing]

case class Cons[+A](h: () => A, t: () => FStream[A]) extends FStream[A]

object FStream {
  def cons[A](hd: => A, tl: => FStream[A]): FStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: FStream[A] = Empty

  def apply[A](as: A*): FStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8
  def constant[A](a: A): FStream[A] = {
    lazy val gen: FStream[A] = cons(a, gen)
    gen
  }

  // Exercise 5.9
  def from(n: Int): FStream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fib: FStream[Int] = {
    def go(first: Int, second: Int): FStream[Int] = {
      cons(first, go(second, first + second))
    }
    go(0, 1)
  }

  // exercise 5.11
  // Write a more general stream building function called
  // unfold. It takes an initial state and a function for
  // producing both the next state and the next value generated
  // in the stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): FStream[A] = {
    f(z).map {
      case (a, s) => cons(a, unfold(s)(f))
    }.getOrElse(empty[A])
  }

  // Exercise 5.12
  def constant2[A](a: A): FStream[A] =
    unfold(a) {
      s => Some(s, s)
    }

  def from2(n: Int): FStream[Int] =
    unfold(n) {
      s => Some(s, s + 1)
    }

  def fib2: FStream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }

}
