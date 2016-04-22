package chapters

/**
  * Created on 2016-04-09.
  */
object Monoids {

  trait Monoid[A] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: A, y: A): A
    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: String, y: String): String = x + y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: List[A], y: List[A]): List[A] = x ++ y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: List[A] = List.empty[A]
  }

  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: Int, y: Int): Int = x + y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: Int, y: Int): Int = x * y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: Boolean, y: Boolean): Boolean = x || y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: Boolean, y: Boolean): Boolean = x && y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: Boolean = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: Option[A], y: Option[A]): Option[A] = x.orElse(y) // x.flatMap(_ => y)

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: Option[A] = None
  }

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    def op(x: A => A, y: A => A): A => A = x andThen y

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: A, y: A): A = m.op(y, x)

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: A = m.zero
  }

  // Exercise 10.5
  def foldMap[A, B](as: Seq[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero){
      (b, a) => m.op(b, f(a))
    }

  // Exercise 10.6
  // turn f into a function that returns an endo function ie.  A => B => B
  // We can then create a monoid that combines endo functions of type B
  // We pass in each element A as we combine the endo functions via composition
  // The result of foldmap is an endo function that takes an initial value and transforms it to an end value of type B
  // each transformation step (func composition) is curried with an element of the list
  // Its like building out all the combination steps and running them at the end
  def foldRight[A, B](l: List[A], zero: B)(f: (A, B) => B): B =
    foldMap(l, endoMonoid[B])(f.curried)(zero)

  // Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v.head)
    else {
      val mid = v.length / 2
      val (l,r) = v.splitAt(mid)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // Exercise 10.16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: (A, B) = (A.zero, B.zero)
  }

  // Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B]{
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: (A) => B, y: (A) => B): (A) => B = (a: A) => B.op(x(a), y(a))

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: (A) => B = (a:A) => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: Map[K, V], y: Map[K, V]): Map[K, V] =
      (x.keySet ++ y.keySet).foldLeft(zero) {
        (b, a) => b.updated(a, V.op(x.getOrElse(a, V.zero), y.getOrElse(a, V.zero)))
      }

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: Map[K, V] = Map.empty[K, V]
  }

  def bagMonoid[A]: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)

  // Exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, bagMonoid[A])(a => Map(a -> 1))

  // Idea compare two numbers in (x, bool) (y, bool), return the largest of the two along with if x was smaller than y
  // if x or y was already false then we also return false
  // This operation is associative
  // op( (1, true), op((2, true), (3, true))) ==  op( (1, true), (3, true)) == (3, true)
  // op( op((1, true), (2, true)), (3, true))) == op( (2, true), (3, true)) == (3, true)
  // The identity would be (Int.MinValue, true) since the boolean operation is AND, true is the identity
  // Since the number result that is returned from op is the max of the two numbers, MinValue suffices as the identity
  val orderedMonoid = new Monoid[(Int, Boolean)] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: (Int, Boolean), y: (Int, Boolean)): (Int, Boolean) = (Math.max(x._1, y._1), x._1 <= y._1 && x._2 && y._2)

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: (Int, Boolean) = (Int.MinValue, true)
  }
  // Exercise 10.9
  // Use foldMap to detect whether a given IndexedSeq[Int] is ordered
  def isOrdered(l: IndexedSeq[Int]): Boolean = {
    foldMap(l, orderedMonoid)(a => (a, true))._2
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // Law [Associativity]: satisfies op(op(x,y), z) == op(x, op(y,z))
    override def op(x: WC, y: WC): WC = (x, y) match {
      case (Stub(cs), Stub(cs2)) => Stub(cs + cs2)
      case (Stub(cs), Part(l, w, r)) => Part(cs + l, w, r)
      case (Part(l, w, r), Stub(cs)) => Part(l, w, r + cs)
      case (Part(l, w, r), Part(l2, w2, r2)) => Part(l, w + w2 + (if (l2.isEmpty && r.isEmpty) 0 else 1), r2)
    }

    // Law [Identity]: satisfies op(zero, x) == op(x, zero)
    override def zero: WC = Stub("")
  }

  //Exercise 10.11
  def wc(content: String) = {
    def get(s: Char): WC = s match {
      case ' ' => Part("", 0, "")
      case w => Stub(w.toString)
    }
    foldMapV(content, wcMonoid)(get) match {
      case Stub(w) => 1 min w.length
      case Part(l, w, r) => w + (1 min l.length) + (1 min r.length)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid[B]))(z)
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] =
      foldRight(fa)(List.empty[A])(_ :: _)
  }

  // Exercise 10.12
  object ListFoldable extends Foldable[List] {

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldMap(as)(a => List(f(a)))(listMonoid)

  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {

    def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = super.foldLeft(as)(z)(f)
  }

  // Exercise 10.14
  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
      as match {
        case None => mb.zero
        case Some(a) => f(a)
      }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }

  }


}
