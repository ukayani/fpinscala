package chapters

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

/**
  * Created on 2016-04-04.
  */
object Parallel {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  object Par {

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isCancelled: Boolean = false
      def get(timeout: Long, unit: TimeUnit): A = get
      def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      def isDone: Boolean = true
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // 7.4
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      sequence(ps.map(asyncF(f)))

    // Exercise 7.5
    def sequence1[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List.empty[A])) {
        (a, b) => map2(a, b)(_ :: _)
      }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case Nil => unit(Nil)
      // run the recursive step on a seperate thread, effectively makes the
      // call tail recursive since we arent building out a call stack on one thread
      case h::t => map2(h, fork(sequence(t)))(_ :: _)
    }

    // Exercise 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

    // Exercise 7.7
    // Given map(y)(id) == y, its a free theorem that
    // map(map(y)(g))(f) == map(y)(f compose g)

    // From map(y)(id) == y, we know that
    // map(unit(x))(g) == unit(g(x))

    // let y = unit(x)
    // then it holds that
    // map(map(unit(x))(g))(f) == map(unit(g(x)))(f)
    // applying the same law again, it holds that
    // map(unit(g(x)))(f) == unit(f(g(x))
    // unit(f(g(x))) == unit(f compose g (x))
    // let  h = f compose g
    // then
    // unit(f(g(x))) == unit(h(x))
    // then it holds that
    // unit(h(x)) == map(unit(x))(h)
    // if we substitute  unit(x) for our original  y = unit(x)
    // we get
    // map(unit(x))(h) == map(y)(h) == map(y)(f compose g)
    // hence
    // map(map(y)(g))(f) == map(y)(f compose g)

    // Exercise 7.9
    // Show that any fixed size thread pool can be made to deadlock given
    // this implementation of fork
    // Given a thread pool with a fixed size of N
    // If we nest  N + 1 calls of fork, we will result in a deadlock


    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    // Exercise 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      es => {
        val index = run(es)(n).get
        run(es)(choices(index))
      }
    }

    def choice1[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(a => if (a) 1 else 0))(List(t, f))

    // Exercise 7.12
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        val k = run(es)(key).get
        run(es)(choices(k))
      }

    // Exercise 7.13
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val index = run(es)(pa).get
        run(es)(choices(index))
      }

    def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(n)(choices)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(cond)(a => if (a) t else f)

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      es => {
        val a = run(es)(pa).get
        run(es)(f(a))
      }

    // Exercise 7.14
    def join[A](a: Par[Par[A]]): Par[A] =
      es => {
        val p = run(es)(a).get
        run(es)(p)
      }

    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(identity)
  }
}
