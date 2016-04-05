object Opt {

  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) this else None)

    def lift[A, B](f: A => B): Option[A] => Option[B] =
      _ map f
  }

  // Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap {
      m => mean(xs.map(x => math.pow(x - m, 2)))
    }
  }
  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap { av =>
      b map(f(av, _))
    }


  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::xs => x flatMap {xx => sequence(xs) map(xss => xx :: xss)}
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight(Some(Nil:List[A]):Option[List[A]]) {
      (a,b) => a flatMap { a1 =>
        b map { b1 =>
          a1 :: b1
        }
      }
    }

  // Exercise 4.5
  // Traverse a list and apply the function f to each element
  // the function f returns an option
  // if the function f fails (returns none) on any element in the list,
  // the entire result is None
  // otherwise the result is an optional list of results

  def traverse[A, B](s: List[A])(f: A => Option[B]): Option[List[B]] = {
    // loop over the list, fold right (so we can build out a new list by consing)
    // if the list is empty, return a Some(Nil) of the correct type
    // otherwise, for each element, flatmap it and add it to the head of the list
    // returned by doing traverse on all elements to its right
    s.foldRight(Some(Nil:List[B]): Option[List[B]]) {
      (l, r) => f(l) flatMap { b =>
        r map { rV =>
          b :: rV
        }
      }
    }
  }
  // we can also use map2 since we want to apply a function to two optional values
  def traverse2[A, B](s: List[A])(f: A => Option[B]): Option[List[B]] = {
    s.foldRight(Some(Nil:List[B]): Option[List[B]]) {
      (l, r) => map2(f(l), r)(_ :: _)
    }
  }

  def sequence3[A](l: List[Option[A]]): Option[List[A]] =
    traverse(l)(identity)
}
import Opt._


Some(1).orElse(Some(2))

variance(List(3, 4, 7, 10)) == Some(7.5)
map2(Some(1), None)((a: Int, b: Int) => a + b)
sequence3(List(Some(1), Some(2)))