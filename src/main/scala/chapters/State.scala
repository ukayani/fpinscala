package chapters

/**
  * Created on 2016-04-04.
  */
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List())) {
      (a, b) => a.map2(b)(_ :: _)
    }
}

case class State[S, +A](run: S => (A, S)) {

  // Exercise 6.10
  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap {
      a => State.unit(f(a))
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap {
      a => sb.map(b => f(a, b))
    }
}