package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int =
     tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
     }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B, g: (Tree[A], Tree[A]) => B): B = {
    tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(l, r)
    }
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold(tree)({_: A => 1} , {(l: Tree[A], r: Tree[A]) => sizeWithFold(l) + sizeWithFold(r) + 1})
  }

  def maximumWithFold(tree: Tree[Int]): Int = {
    fold(tree)(identity, {(l: Tree[Int], r: Tree[Int]) => maximum(l) max maximum(r)})
  }

  def depthWithFold[A](tree: Tree[A]): Int = {
    fold(tree)({ _: A => 0 }, {(l: Tree[A], r: Tree[A]) => (depthWithFold(l) max depthWithFold(r)) + 1})
  }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)({ v: A => Leaf(f(v)) }, {(l: Tree[A], r: Tree[A]) => Branch(mapWithFold(l)(f), mapWithFold(r)(f))})
  }

}