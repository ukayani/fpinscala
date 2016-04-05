sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) =>  f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)
  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)
  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((a,b) => 1 + (a max b))
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}

import Tree._

size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))))
depth(Branch(Leaf(1), Leaf(2)))
map(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))))(_ + 2)
fold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))((v) => 1)(1 + _ + _)
fold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))((v) => Leaf(v + 1):Tree[Int])(Branch(_, _))