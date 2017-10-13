package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(x) => 1
      case Branch(l, r) => 1 + (size(l) + size(r))
    }
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(x) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(f2: (Tree[A], Tree[A]) => B): B =
    tree match {
      case Leaf(x) => f(x)
      case Branch(l, r) => f2(l,r)
    }

  def size2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l,r) => 1 + size2(l) + size2(r))
  }

  def maximum2(tree: Tree[Int]): Int = {
    fold(tree)(x => x)((l,r) => maximum2(l) max maximum2(r))
  }

  def depth2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 0)((l,r) => 1 + (depth2(l) max depth2(r)))
  }

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A,Tree[B]](tree)(x => Leaf(f(x)))((l,r) => Branch(map2(l)(f), map2(r)(f)))
  }

}
