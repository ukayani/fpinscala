package chapters

import chapters.Monads.{Functor, Monad}
import chapters.Monoids.{Foldable, Monoid}

import scala.language.reflectiveCalls
import scala.{Stream => SStream}

/**
  * Created on 2016-04-19.
  */
object Applicatives {

  trait Applicative[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit[A => B => C](f.curried))(fa))(fb)

    // Exercise 12.2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fa, fab)((a, fb) => fb(a))

    def map2_apply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit[A => B => C](f.curried))(fa))(fb)

    def map2_apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map[A, B => C](fa)(f.curried))(fb)

    // Exercise 12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)

    // Derived combinators
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B])) {
        (a, b) => map2(f(a), b)(_ :: _)
      }

    // Exercise 12.1
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((a, b) => (a, b))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms.foldRight(unit(List.empty[A])) {
        (a, b) => map2(f(a), b) {
          // unwrap f(a) to determine if we should include a or not
          (include, b1) => if (include) a :: b1 else b1
        }
      }

    // Exercise 12.8
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
          (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }

    // Exercise 12.9
    def compose[G[_]](G: Applicative[G]) = {
      val self = this

      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fa, fb)(G.map2(_, _)(f))
      }
    }

    // Exercise 12.12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldLeft(unit(Map.empty[K, V])) {
        (b, kfv) => map2(b, kfv._2)((b1, v) => b1.updated(kfv._1, v))
      }
  }

  object Applicative {
    type Const[A, B] = A

    implicit def monoidApplicative[M](M: Monoid[M]) =
    // we partially apply the Const type constructor, where f[x] = M
      new Applicative[({ type f[x] = Const[M, x] })#f] {
        def unit[A](a: => A): M = M.zero
        override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
      }

  }

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
    def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
      sequence(map(fa)(f))
    def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
      traverse(fma)(ma => ma)

    // Exercise 12.14
    // implement map in terms of traverse
    type Id[A] = A
    val idMonad = new Monad[Id] {
      override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = f(ma)

      override def unit[A](a: => A): Id[A] = a
    }

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)(idMonad)

    import Applicative._

    def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
      traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

    def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monads.Monad.stateMonad)

    import State._
    def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
      traverseS(ta)((a: A) => (for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i))).run(0)._1


    def toList_[A](fa: F[A]): List[A] =
      traverseS(fa)((a: A) => (for {
        l <- get[List[A]]
        _ <- set(a :: l)
      } yield ())).run(List.empty[A])._2.reverse

    def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _  <- set(s2)
      } yield b)).run(s)

    override def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    // Exercise 12.16
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1

    // Exercise 12.17
    override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(fa, z)((a, b) => ((), f(b,a)))._2


    // Exercise 12.18
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                              (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))

    // Exercise 12.19
    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[M[_]:Applicative, A, B](fa: F[G[A]])(f: (A) => M[B]): M[F[G[B]]] =
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }



  }


  object MonadComposition {
    // Exercise 12.20
    def composeM[F[_],G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
      new Monad[({type f[x] = F[G[x]]})#f] {

        override def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = {
          F.flatMap(ma)((ga: G[A]) => F.map(T.traverse(ga)(f))((ggb: G[G[B]]) => G.join(ggb)))
        }

        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      }
  }

  object Traverse {
    val listTraverse = new Traverse[List] {
      override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
        as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
    }
  }




  val streamApplicative = new Applicative[SStream]  {
    override def unit[A](a: => A): SStream[A] = SStream.continually(a)

    override def map2[A, B, C](fa: SStream[A], fb: SStream[B])(f: (A, B) => C): SStream[C] =
      fa zip fb map f.tupled

    // Exercise 12.4
    // Sequence transposes the list, it takes a list of rows of infinite length and produces
    // a list column values at each row position
    //def sequence[A](fas: List[SStream[A]]): SStream[List[A]] = super.sequence(fas)


  }

  // Exercise 12.5
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    override def unit[A](a: => A): Either[E, A] = Right(a)
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  // Exercise 12.6
  def applicativeValidation[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h, t), Success(b)) => Failure(h, t)
        case (Success(a), Failure(d,s)) => Failure(d,s)
        case (Failure(h, t), Failure(d, s)) => Failure(h, t ++: d +: s)
      }
  }



}
