// Exercise 2.1
// Write a tail recursive function that computes fibonacci numbers
def fib(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, a: Int, b: Int): Int = {
    if (n == 0) a
    else
      // get the next number
      loop(n - 1, b, a + b)
  }
  // start with the first two numbers
  loop(n, 0, 1)
}
// print first 8 fib numbers
for (i <- 0 to 7) { println(fib(i))}


// Exercise 2.2
// Implement isSorted which checks whether an Array[A] is sorted
// according to a given comparison function

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(i: Int): Boolean = {
    if (i == as.length - 1) true
    else if (ordered(as(i), as(i + 1))) loop(i + 1) else false
  }
  loop(0)
}

isSorted(Array(1, 2, 3), (a: Int, b:Int) => a < b)
isSorted(Array(1), (a: Int, b:Int) => a < b)
isSorted(Array(1, 2), (a: Int, b: Int) => a < b)
isSorted(Array(1, 2, 1), (a: Int, b: Int) => a < b)


// Exercise 2.3
// Implement a currying function which converts a function f of two arguments into a function
// of one argument that partially applies f

def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

val curriedSum = curry((a: Int, b: Int) => a + b)
curriedSum(1)(1)

// Exercise 2.4
// Implement uncurry, which undoes what the curry function does
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

val sum = uncurry(curriedSum)
sum(1,1)

// Exercise 2.5
// Implement the higher order function that composes two functions

def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

val increment = (a: Int) => a + 1
val square = (b: Int) => b * b

compose(square, increment)(1) // 4
compose(square, increment)(2) // 9

