import chapters.LocalEffects._

val p = new RunnableST[(Int, Int)] {
  override def apply[S]: ST[S, (Int, Int)] =
    for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a,b)
}

ST.runST(p)

val d = new RunnableST[List[Int]] {
  // Since we are polymorphic in the type S, even if A = STRef[T, A], we are guaranteed that T cannot be the same as S
  override def apply[S]: ST[S, List[Int]] = for {
    r1 <- STArray.fromList(List(1,2,3))
    x <- r1.read(0)
    _ <- r1.write(0, x + 1)
    l <- r1.freeze
  } yield l
}

ST.runST(d)

val l = List(1,4,2,5,3,1)
quicksort(l)

val m = new RunnableST[Map[String, Int]] {
  // Since we are polymorphic in the type S, even if A = STRef[T, A], we are guaranteed that T cannot be the same as S
  override def apply[S]: ST[S, Map[String, Int]] =
    for {
      m1 <- STHashMap.empty[S, String, Int]
      _ <- m1.write("Bob", 1)
      age <- m1.read("Bob")
      _ <- m1.write("Jim", age + 5)
      m <- m1.freeze
    } yield m
}

ST.runST(m)