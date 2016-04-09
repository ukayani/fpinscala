import chapters.PropertyTesting.Gen
import chapters.PropertyTesting.Gen._
import chapters.PropertyTesting.Prop._

val smallInt = Gen.choose(-10, 10)
val maxProp = forAll(listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}

// Exercise 8.14
val sortedProp = forAll(listOf1(smallInt)) { ns =>
  val sorted = ns.sorted
  (sorted.isEmpty || sorted.tail.isEmpty ||
    !sorted.zip(sorted.tail).exists {
      case (a, b) => a > b
    }) &&
    !ns.exists(!sorted.contains(_)) &&
    !sorted.exists(!ns.contains(_))
}

run(maxProp)
run(sortedProp)

val l = List(1,2,3,4,5)
l.zip(l.tail)