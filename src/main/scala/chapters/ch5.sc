
object Streams {

  sealed trait Stream[+A] {
    import Stream._
    // Exercise 5.1
    def toList: List[A] = {
      @annotation.tailrec
      def go(acc: List[A], l: Stream[A]): List[A] =
        l match {
          case Empty => acc.reverse
          case Cons(h,t) => go(h() :: acc, t())
        }
      go(Nil, this)
    }

    // Exercise 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n -1)
      case _ => this
    }

    // Exercise 5.3
    def takeWhile2(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile2(p))
      case _ => empty
    }

    def foldRight[B](z: B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false){(a, b) => p(a) || b}

    // Exercise 5.4
    def forall(p: A => Boolean): Boolean = {
      !this.exists((a: A) => !p(a))
    }

    def forall2(p: A => Boolean): Boolean =
      foldRight(true){
        (a, b) => p(a) && b
      }

    // Exercise 5.5
    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A]){ (a, b) =>
        if (p(a)) cons(a, b) else empty
      }

    // Exercise 5.6
    def headOption: Option[A] =
      foldRight(None: Option[A]) {
        (a, _) => Some(a)
      }

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B]) {
        (a, b) => cons(f(a), b)
      }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) {
        (a, b) => if (p(a)) cons(a, b) else b
      }

    def append[B >: A](l: => Stream[B]): Stream[B] =
      foldRight(l){
        (a, b) => cons(a, b)
      }

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) {
        (a, b) => f(a) append b
      }

    // Exercise 5.13
    def map2[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }

    def take2(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
        case _ => None
      }

    def takeWhile3(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }

    def zipWith[B, C](l: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, l)) {
        case (Cons(h, t), Cons(bh, bt)) => Some(f(h(), bh()), (t(), bt()))
        case _ => None
      }

    def zipAll[B](l: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, l)) {
        case (Cons(h, t), Cons(bh, bt)) => Some((Some(h()), Some(bh())), (t(), bt()))
        case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty[B]))
        case (_, Cons(bh, bt)) => Some((None, Some(bh())), (empty[A], bt()))
        case _ => None
      }

    // Exercise 5.14
    def startsWith[B](s: Stream[B]): Boolean =
      zipAll(s).foldRight(true) {
        (a, b) => a match {
          case (Some(a1), Some(a2)) if a1 == a2 => b
          case (Some(a1), None) => true
          case _ => false
        }
      }

    def startsWith2[B](s: Stream[B]): Boolean =
      zipAll(s).takeWhile(_._2.isEmpty).forall({
        case (a1, b1) => a1 == b1
      })

    // Exercise 5.15
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Cons(h, t) if t() == Empty => Some(empty[A], t())
        case l @ Cons(h, t) => Some(l, t())
        case _ => None
      }

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    // Exercise 5.16
    // Implement a function like fold which returns intermediate results
    // as a stream
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      // The zero case would be a stream with the zero element
      foldRight(cons(z, empty[B])) {
        // note: we can safely call b.headOption.get since b can never be empty
        // we start b off as a non empty stream and keep prepending to it
        // we also retain the lazy-ness of the list since f's second arg is by-name
        // so the b.headOption.get may not even get called
        (a, b) => cons(f(a, b.headOption.get), b)
      }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]:Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

    // Exercise 5.8
    def constant[A](a: A): Stream[A] = {
      lazy val gen: Stream[A] = cons(a, gen)
      gen
    }

    // Exercise 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // Exercise 5.10
    def fib: Stream[Int] = {
      def go(first: Int, second: Int): Stream[Int] = {
        cons(first, go(second, first + second))
      }
      go(0,1)
    }

    // exercise 5.11
    // Write a more general stream building function called
    // unfold. It takes an initial state and a function for
    // producing both the next state and the next value generated
    // in the stream
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z).map {
          case (a, s) => cons(a, unfold(s)(f))
        }.getOrElse(empty[A])
    }

    // Exercise 5.12
    def constant2[A](a: A): Stream[A] =
      unfold(a) {
        s => Some(s, s)
      }

    def from2(n: Int): Stream[Int] =
      unfold(n) {
        s => Some(s, s + 1)
      }

    def fib2: Stream[Int] =
      unfold((0, 1)) {
        case (a, b) => Some(a, (b, a + b))
      }

  }
}
import Streams._
chapters.Stream(1,2,3,4,5).drop(2).toList
chapters.Stream(2,4,2,6).forall(_ % 2 == 0)
chapters.Stream(1,2).headOption
chapters.Stream(1,2,3,4).map(_ + 1).take(2).toList
chapters.Stream(1,2,3,4).filter(_ % 2 == 0)

val ones: chapters.Stream[Int] = chapters.Stream.cons(1, ones)
ones.take(10).toList
chapters.Stream.constant(1).take(10).toList
chapters.Stream.fib.take(10).toList
chapters.Stream.constant2(2).take(4).toList
chapters.Stream.from(2).take(3).toList
chapters.Stream.fib2.take(10).toList
chapters.Stream(1,2,3,4).map2(_ + 2).take2(2).toList
chapters.Stream(1,2,3).zipWith(chapters.Stream(2,3))(_ + _).toList
chapters.Stream(1,2,3).zipAll(chapters.Stream(2, 3)).toList
chapters.Stream(1,2,3,4).startsWith2(chapters.Stream(1,2))
chapters.Stream(1,2,3,4).tails.map(_.toList).toList
chapters.Stream(1,2,3,4,5).hasSubsequence(chapters.Stream(3,4))
chapters.Stream(1,2,3).scanRight(0)(_ + _).toList