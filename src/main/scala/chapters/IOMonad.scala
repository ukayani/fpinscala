package chapters

import chapters.Monads.Monad

/**
  * Created on 2016-04-28.
  */
object IOMonad {

  // Exercise 13.1
  sealed trait Free[F[_], A] { self =>
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(self, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {

    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma flatMap f
  }

  // Exercise 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(y, g) => runTrampoline(y flatMap {a => g(a) flatMap f})
    }
  }

  // Exercise 13.3
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap { a => f(a) flatMap g })
    case FlatMap(Return(a), f) => step(f(a))
    case _ => a
  }
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(x, f) => x match {
      case Suspend(s) => F.flatMap(s){a => run(f(a))}
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

}

object TailRecMonad {
  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    override def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap { _ => a }

  }

  @annotation.tailrec
  def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  // A specialized reader monad
  case class ConsoleReader[A](run: String => TailRec[A]) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => run(r).map(f))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => FlatMap(run(r), f andThen(_.run(r))))
  }

  // A specialized reader monad
//  case class ConsoleReader[A](run: String => A) {
//    def map[B](f: A => B): ConsoleReader[B] =
//      ConsoleReader(r => f(run(r)))
//    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
//      ConsoleReader(r => f(run(r)).run(r))
//  }
//  object ConsoleReader {
//    implicit val monad = new Monad[ConsoleReader] {
//      def unit[A](a: => A) = ConsoleReader(_ => a)
//      def flatMap[A,B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
//    }
//  }

  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => Return(a))
      override def flatMap[A,B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
    }
  }
}
