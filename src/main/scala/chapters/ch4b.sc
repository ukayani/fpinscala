object Eithers {
  sealed trait Either[+E, +A] {
    // Exercise 4.06
    def map[B](f: A => B):Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => this
      case Left(e) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        b1 <- b
      } yield f(a, b1)

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def Try[A](a: => A):Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  // Exercise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    as.foldRight(Right(Nil: List[B]): Either[E, List[B]]) {
      (a, b) => f(a).map2(b)(_ :: _)
    }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(identity)
}

import Eithers._

traverse(List(1, 2, 3, 4))(a => if (a < 0) Left("Damn") else Right(a))