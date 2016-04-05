object States {

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

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  // Exercise 6.11
  // Implement a finite state machine for dispensing candy
  //
  // 1. Inserting a coin into a locked machine will cause it to unlock if there's
  // any candy left
  //
  // 2. Turning the knob on an unlocked machine will cause it to dispense candy
  // and become locked
  //
  // 3. Turning the knob on a locked machine or inserting a coin into an unlocked machine
  // does nothing
  //
  // 4. A machine that is out of candy ignores all inputs
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {
    def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
      // Returns a single state transition function based on the given input i
      // ie. depending on the value of i, we will modify the state of the machine
      def single(i: Input): State[Machine, Unit] =
        State.modify {
          m: Machine => {
            (m, i) match {
              // 4. Out of candies, return same state
              case (Machine(_, 0, _), _) => m
              // 1. Inserting coin into locked machine, increment coin count and unlock
              case (Machine(true, cnd, c), Coin) => Machine(locked = false, cnd, c + 1)
              // 2. Turn knob on unlocked machine, decrement candy and lock machine again
              case (Machine(false, cnd, c), Turn) => Machine(locked = true, cnd - 1, c)
              // 3. Turning on a locked machine or inserting on unlocked machine
              // Note: we could have combined the last two cases into a general catch all
              // Also, if we put guards on the candy count in the above two cases, we could
              // omit the first case
              case (Machine(true, _, _), Turn) => m
              case (Machine(false, _, _), Coin) => m
            }
          }
        }

      for {
        // Take all inputs and produce state transition functions from one input, to the next
        // This is slightly unintuitive as we aren't really passing the inputs through the
        // machine but more so we are constructing a series of transition functions encapsulating
        // the modification of the machine state based on the sequence of inputs
        // we then feed an initial state through this pipeline and it goes from one
        // function to the next, each modifying the state
        // the final state is the result of running all state actions (which are based on the input)
        g <- State.sequence(inputs.map(single))
        // at this point we are doing a flatMap on State[Machine, List[Unit]]
        // we ignore the List[Unit] and instead retrieve the Machine (just the state portion)
        m <- State.get
        // we then do a map (which gets the state as input)
      } yield (m.candies, m.coins)
    }
  }

}

import States._

Machine.simulate(List(Coin, Turn, Coin, Turn, Coin, Turn,
  Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 0))

