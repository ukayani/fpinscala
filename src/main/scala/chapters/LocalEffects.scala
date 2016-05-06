package chapters
/**
  * Created on 2016-05-04.
  */
object LocalEffects {

  sealed trait ST[S,A] { self =>
    protected def run(s: S): (A,S)
    def map[B](f: A => B): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
    def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S,A](a: => A) = {
      lazy val memo = a
      new ST[S,A] {
        def run(s: S) = (memo, s)
      }
    }
    def runST[A](st: RunnableST[A]): A =
      st[Unit].run(())._1

    def noop[S]: ST[S, Unit] = apply(())
  }

  sealed trait STRef[S,A] {
    protected var cell: A
    def read: ST[S,A] = ST(cell)
    def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    // Note: The only way to create an STRef is by creating an ST and embedding the STRef inside
    // This ensures that mutable references cannot be created outside of our ST context
    // We also ensure that the S type in ST is the same as whats in an STRef, hence it acts as a tag, ensuring
    // That an STRef cant exist inside of an ST with a different type of State
    def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
      var cell = a
    })
  }

  trait RunnableST[A] {
    // Since we are polymorphic in the type S, even if A = STRef[T, A], we are guaranteed that T cannot be the same as S
    // Since S can be any type.
    def apply[S]: ST[S,A]
  }

  // Scala requires an implicit Manifest for constructing arrays.
  sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S,Int] = ST(value.size)

    // Write a value at the give index of the array
    def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    // Read the value at the given index of the array
    def read(i: Int): ST[S,A] = ST(value(i))

    // Turn the array into an immutable list
    def freeze: ST[S,List[A]] = ST(value.toList)

    def swap(i: Int, j: Int): ST[S,Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

    // Exercise 14.1
    def fill(xs: Map[Int, A]): ST[S, Unit] =
      xs.foldLeft(ST[S, Unit](())) {
        (b, a) => b flatMap(_ => write(a._1, a._2))
      }
  }

  object STArray {
    // Construct an array of the given size filled with the value v
    def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
      ST(new STArray[S,A] {
        lazy val value = Array.fill(sz)(v)
      })

    def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
      ST(new STArray[S,A] {
        lazy val value = xs.toArray
      })
  }

  sealed trait STHashMap[S, K, V] {

    protected def _value: scala.collection.mutable.HashMap[K, V]
    def size: ST[S, Int] = ST(_value.size)

    def write(key: K, value: V): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        _value += (key -> value)
        ((), s)
      }
    }

    def read(key: K): ST[S, V] = new ST[S, V] {
      def run(s: S): (V, S) = {
        (_value(key), s)
      }
    }

    def contains(key: K): ST[S, Boolean] = new ST[S, Boolean] {
      def run(s: S): (Boolean, S) = {
        (_value.contains(key), s)
      }
    }

    def freeze: ST[S, Map[K, V]] = new ST[S, Map[K, V]] {
      def run(s: S): (Map[K, V], S) = {
        (_value.toMap, s)
      }
    }

    def keys: ST[S, Set[K]] = new ST[S, Set[K]] {
      def run(s: S): (Set[K], S) = {
        (_value.keySet.toSet, s)
      }
    }

    def values: ST[S, Iterable[V]] = new ST[S, Iterable[V]] {
      def run(s: S): (Iterable[V], S) = {
        (_value.values, s)
      }
    }
  }

  object STHashMap {
    def empty[S, K, V]: ST[S, STHashMap[S, K, V]] =
      ST(new STHashMap[S, K, V] {
        lazy val _value = scala.collection.mutable.HashMap.empty[K, V]
      })

    def fromMap[S, K, V](map: Map[K, V]): ST[S, STHashMap[S, K, V]] =
      ST(new STHashMap[S, K, V] {
        lazy val _value = scala.collection.mutable.HashMap(map.toSeq:_*)
      })
  }

  // Exercise 14.2
  def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = {

    // Given a pivot value put all elements smaller than the value on the very left of the array and all elements
    // greater on the right
    // Essentially acts like a loop over the range l to r which swaps elements to the left side if they are smaller
    // than the pivot
    // Note: we must stay in the context of ST in order to use this operation in the chain below
    def swapRange(pivotVal: Int, jRef: STRef[S, Int]): ST[S, Unit] =
      (l until r).foldLeft(ST.noop[S]) {
        (st, i) => for {
          _ <- st
          elem <- arr.read(i)
          _ <- if (elem < pivotVal) for {
            j <- jRef.read
            _ <- arr.swap(i, j)
            _ <- jRef.write(j + 1)
          } yield () else ST.noop[S]
        } yield ()
      }

    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      jRef <- STRef(l)
      _ <- swapRange(pivotVal, jRef)
      j <- jRef.read
      _ <- arr.swap(j, r)
    } yield j

  }

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] =
    if (l < r) for {
        pi <- partition(a, l, r, l + (r - l) / 2)
        _ <- qs(a, l, pi - 1)
        _ <- qs(a, pi + 1, r)
    } yield () else ST.noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

  def quicksort2(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}
