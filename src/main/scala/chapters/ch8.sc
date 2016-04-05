import chapters.PropertyTesting.Gen
import chapters.PropertyTesting.Gen._
import chapters.PropertyTesting.Prop._

val smallInt = Gen.choose(-10, 10)
val maxProp = forAll(listOf(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}

run(maxProp)