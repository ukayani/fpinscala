package chapters

import chapters.Applicatives.Applicative

/**
  * Created on 2016-04-16.
  */
object Monads {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
  }

  trait Monad[F[_]] extends Applicative[F] {

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

//    // Exercise 11.3
//    def sequence[A](lma: List[F[A]]): F[List[A]] =
//      lma.foldRight(unit(List.empty[A])) {
//        (a, b) => map2(a, b)(_ :: _)
//      }
//
//    // We can also implement sequence in terms of traverse
//    def sequence_2[A](lma: List[F[A]]): F[List[A]] =
//      traverse(lma)(identity)
//
//    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
//      la.foldRight(unit(List.empty[B])) {
//        (a, b) => map2(f(a), b)(_ :: _)
//      }
//
//    // Exercise 11.4
//    // We can build up a list of F[A] and use sequence to combine them
//    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
//      sequence(List.fill(n)(ma))


    // Exercise 11.5
    // The replicateM for the List monad takes a list A produces a List of List A repeated n times, it then
    // combines this list into single monadic value List of List[A]
    // The replicate M for the Option monad takes an option A and produces a new Option B , if option A had a value,
    // Option B contains that value repeated n times in a list
    // In general, replicateM takes a list of size n of monad A and combines it into a single monad with the values
    // combined

//    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    // Exercise 11.6
//    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
//      ms.foldRight(unit(List.empty[A])) {
//        (a, b) => map2(f(a), b) {
//          // unwrap f(a) to determine if we should include a or not
//          (include, b1) => if (include) a :: b1 else b1
//        }
//      }

    // Exercise 11.7
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    // Exercise 11.8
    def flatMapFromCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose(identity:F[A] => F[A], f)(ma)

    // Exercise 11.12
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

    // Exercise 11.13
    def flatMapFromJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    // Exercise 12.11
    // We cant implement compose on monad
    // If we were to flatmap on ma we get a type G[A], to get a we need to call Monad[G]'s flatmap on the type G[A]
    // this would yield us A, however f is not a valid function for flatmap of Monad[G] since the outer most wrapping
    // is of type F
//    def compose[G[_]](G: Monad[G]) = {
//      val self = this
//      new Monad[({type f[x] = F[G[x]]})#f] {
//        override def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] =
//
//
//        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
//      }
//    }

  }

  object Monad {
    // Exercise 11.17
    case class Id[A](value: A) {
      def map[B](f: A => B): Id[B] = this.copy(value = f(value))
      def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    }

    object IdMonad extends Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
    }

    // Exercise 11.20
    case class Reader[R, A](run: R => A)

    object Reader {
      // Note ({type f[x] = Reader[R, x]})#f is a type lambda
      // similar to an anonymous function in the type system, we declare an anonymous type with a single
      // type member, where we partially supply one of the type params for Reader (kind of like currying the type constructor)
      // we use the #f syntax to access the inner type f. Similar to how we access the property of an object via dot
      // we access the inner type via #
      def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
        override def unit[A](a: =>A): Reader[R, A] = Reader(r => a)

        override def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
          Reader(
            r => {
              val a = ma.run(r)
              f(a).run(r)
            }
          )

      }
    }

    // Exercise 11.1
    object OptionMonad extends Monad[Option] {

      def unit[A](a: => A): Option[A] = Some(a)

      override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
        ma.flatMap(f)
    }

    object ListMonad extends Monad[List] {
      def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
    }

    // Exercise 11.2

    def stateMonadA[S] = {
      type StateS[A] = State[S, A]

      new Monad[StateS] {
        override def unit[A](a: => A): StateS[A] = State.unit(a)

        override def flatMap[A, B](ma: StateS[A])(f: (A) => StateS[B]): StateS[B] =
          ma flatMap f
      }
    }

    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }


  }



}
