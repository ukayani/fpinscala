object Randomness {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Exercise 6.1
  def nonNegativeInt: Rand[Int] = rng => {
    val (rn, rng2) = rng.nextInt
    if (rn == Int.MinValue) (rn - 1, rng2) else (math.abs(rn), rng2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (rn, rng2) = nonNegativeInt(rng)
    ((rn.toDouble / Int.MaxValue), rng2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (num, rng2) = rng.nextInt
    val (db, rng3) = double(rng2)
    ((num, db), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (num, rngNext) = rng.nextInt
        go(n - 1, num :: acc, rngNext)
      }
    }

    go(count, Nil, rng)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble)

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // Exercise 6.7
  // Implement sequence, for combining a List of transitions into a single
  // transition.
  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs match {
      case Nil => (Nil, rng)
      case h::t => {
        val (a, rng2) = h(rng)
        val (rest, rngNext) = sequence1(t)(rng2)
        (a :: rest, rngNext)
      }
    }
  }
  // Trying a tail rec approach
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def go(l: List[Rand[A]], acc: List[A], rng: RNG): (List[A], RNG) = l match {
      case Nil => (acc, rng)
      case h::t => {
        val (a, rng2) = h(rng)
        go(t, a :: acc, rng2)
      }
    }

    (rng) => go(fs, Nil, rng)
  }

  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] = rng =>
    fs.foldRight((List.empty[A], rng)) {
      (a, b) => {
        val (a1, rng2) = a(b._2)
        (a1 :: b._1, rng2)
      }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) {
      (a, b) => map2(a, b)(_ :: _)
    }

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        // Note: when i + (n - 1) > Int.MaxValue, the addition becomes negative (possibly)
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // Exercise 6.9
  def _map[A, B](s: Rand[A])(f: A => B) : Rand[B] =
    flatMap(s) {
      a => unit(f(a))
    }

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a => _map(rb)(b1 => f(a, b1))
    }

}

import Randomness._

val (pos, rng) = nonNegativeInt(SimpleRNG(1))
val (db, rng2) = double2(rng)
val ((a, b), rng3) = intDouble(rng2)
val (List(d, e, f), rng4) = ints(3)(rng3)
val (g, next) = sequence(List(nonNegativeEven, nonNegativeEven))(rng4)
val (h, next2) = nonNegativeLessThan(6)(next)
10 + Int.MaxValue