import chapters.StreamProcessing.Process
import chapters.StreamProcessing.Process._

val p = lift((i: Int) => i * 2)
p(Stream(1,2,3,4,5)).toList

val t = filter((i: Int) => i % 2 == 0)
t(Stream(1,2,3,4,5,6)).toList

take(3)(Stream(1,2,3,4,5,6)).toList

drop(3)(Stream(1,2,3,4,5,6)).toList

takeWhile((i: Int) => i % 2 == 0)(Stream(2,4,6,3,4,6)).toList

dropWhile((i: Int) => i % 2 == 0)(Stream(2,4,6,3,4,6)).toList

count(Stream(1,1,1,1)).toList

mean(Stream(1,2,3,4,5)).toList

sum2(Stream(1,2,3,4,5)).toList
count2(Stream(1,1,1,1,1,1)).toList


val s = drop[Int](1) |> take[Int](3)
s(Stream(1,2,3,4,5,6,7)).toList


p.zipWithIndex(Stream(1,2,3,4,5,6)).toList

zip(drop[Int](3), identity[Int])(Stream(1,2,3,4,5,6)).toList

mean2(Stream(1,2,3,4,5)).toList

exists((i: Int) => i % 2 == 0)(Stream(1,3,5,6,7,9,11)).toList