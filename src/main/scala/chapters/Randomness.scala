package chapters

/**
  * Created on 2016-03-30.
  */
object Randomness {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def Simple(seed: Long): RNG = SimpleRNG(seed)
  }

  type Rand[+A] = State[RNG, A]

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Exercise 6.1
  def nonNegativeInt: Rand[Int] = State(rng => {
    val (rn, rng2) = rng.nextInt
    if (rn == Int.MinValue) (rn - 1, rng2) else (math.abs(rn), rng2)
  })

  def double: Rand[Double] =
    nonNegativeInt.map(n => n.toDouble / Int.MaxValue)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
    }

  def between(start: Int, end: Int): Rand[Int] =
    nonNegativeLessThan(end - start).map(start + _)
}
