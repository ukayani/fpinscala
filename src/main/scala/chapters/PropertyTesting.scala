package chapters

import java.util.concurrent.Executors

import chapters.Parallel.Par
import chapters.Randomness._

/**
  * Created on 2016-03-30.
  */
object PropertyTesting {

  case class Gen[+A](sample: State[RNG, A]) {

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))
    // Exercise 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(s => listOfN(s))

    def listOfN(n: Int): Gen[List[A]] =
      // Create a sequence of state transitions using the provided generator
      // The resulting transition returns a list of all transitions
      Gen(State.sequence(List.fill(n)(sample)))

    // Exercise 8.10
    def unsized: SGen[A] = SGen(_ => this)

    def **[B](g: Gen[B]): Gen[(A,B)] =
      (this map2 g)((_,_))
  }

  object Gen {

    // Exercise 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(between(start, stopExclusive))

    // Exercise 8.5
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
    def boolean: Gen[Boolean] = Gen(nonNegativeInt.map(_ % 2 == 0))

    // Exercise 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(a => if (a) g1 else g2)

    // Exercise 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      Gen(double).flatMap { d =>
        val p1 = g1._2 / (g2._2 + g1._2)
        if (d < p1) g1._1 else g2._1
      }
    }

    // Exercise 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n))

    // Exercise 8.13
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(1 max n))

    object ** {
      def unapply[A,B](p: (A,B)) = Some(p)
    }
  }

  // Exercise 8.11
  case class SGen[+A](forSize: Int => Gen[A]) {
    def apply(n: Int):Gen[A] = forSize(n)

    def map[B](f: A => B): SGen[B] =
      SGen(forSize andThen (_ map f))

    def flatMap[B](f: A => Gen[B]): SGen[B] =
      SGen(forSize andThen (_ flatMap f))
  }


    //  trait Prop {
  //    def check: Boolean
  //    // Exercise 8.3
  //    def &&(p: Prop): Prop = new Prop {
  //      def check = Prop.this.check && p.check
  //    }
  //
  //  }
  import Prop._
  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    // Exercise 8.9
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) => {
        val left = run(max, n, rng)
        if (left.isFalsified) left
        else
          p.run(max, n, rng)
      }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) => {
        val left = run(max, n, rng)
        if (!left.isFalsified) left
        else
          p.run(max, n, rng)
      }
    }
  }

  object Prop {
    import Gen._
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int

    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }
    case object Proved extends Result {
      def isFalsified = false
    }
    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      override def isFalsified: Boolean = true
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n - 1) / max + 1
        val props: FStream[Prop] =
          FStream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

    def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
      // Generate a stream using the generator
      // Zip the stream with an index so we can determine how many cases already
      // end the stream at n elements and run the predicate on each (ie. the test)
      (n, rng) => randomStream(g)(rng).zip(FStream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
    }

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
    }

    val S = weighted(
      choose(1,4).map(Executors.newFixedThreadPool) -> .75,
      unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) { case s ** a => f(a)(s).get }

    // Exercise 8.16
    // First build a list of varying size (1, 20) that contains numbers from -10, 10
    // Using this list, build out parallel summation computations that produce a single value Par[Int]
    val pIntComplex = choose(-10, -10).listOfN(choose(1,20)).map { l =>
      l.foldLeft(Par.unit(0)) {
        (p, i) => Par.fork { Par.map2(p, Par.unit(i))(_ + _) }
      }
    }

    // Exercise 8.17
    // Express the property about fork from chapter 7, that fork(x) == x
    val forkProp = forAllPar(pIntComplex) {
      x => Par.equal(Par.fork(x), x)
    }

    val isEven = (i: Int) => i % 2 == 0

    val takeWhileProp = forAll(Gen.listOf(choose(0, 10))) {
      l => l.takeWhile(isEven).forall(isEven)
    }
    // Exercise 8.18
    // Come up with properties that takeWhile should satisfy
    // Think of a property expressing the relationship between takeWhile and dropWhile
    val takeWhileDropProp = forAll(Gen.listOf(choose(0, 10))) {
      l => (l.takeWhile(isEven) ++ l.dropWhile(isEven)) == l
    }

    // Generating constant functions
    def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
      g map (i => (s => i))



    // Generate an infinite stream using an initial seed state (RNG)
    def randomStream[A](a: Gen[A])(rng: RNG): FStream[A] =
      FStream.unfold(rng)(s => Some(a.sample.run(s)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s]n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    // convenience method for not passing in max
    def apply(f: (TestCases,RNG) => Result): Prop =
      Prop { (_,n,rng) => f(n,rng) }

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property")
      }
  }
}
