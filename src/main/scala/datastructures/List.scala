package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("the list is empty")
    case Cons(head, tail) => tail
  }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(head, tail) => Cons(a, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x,init(xs))
    case Nil => throw new Exception("the list is empty")
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B):B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x:Int, y:Int) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](ns: List[A]): Int = {
    foldRight(ns, 0)((x, y) => 1 + y)
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
    }

  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_+_)

  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_*_)

  def lengthLeft[A](ns: List[A]): Int =
    foldLeft(ns, 0)((x, y) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as,List[A]())((a,b) => Cons(b, a))

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B):B =
    foldRight(reverse(as), z)((a,z) => f(z,a))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((z,a) => f(a,z))

  def append[A](l: List[A], a: A): List[A] =
    foldLeft(reverse(l), List(a))((x, y) => Cons(y,x))

  def concat[A](l: List[A], l2: List[A]): List[A] =
    foldLeft(l2, l)((acc, headElem) => {
      append(acc, headElem)
    })

  def concat2[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((acc, headElem) => {
      concat(acc, headElem)
    })

  def addOne(l: List[Int]): List[Int] =
    foldLeft(l, List[Int]())((acc, headElem) => {
      append(acc, headElem + 1)
    })

  def doubleToString(l: List[Double]): List[String] =
    foldLeft(l, List[String]())((acc, headElem) => {
      append(acc, headElem.toString)
    })

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, List[B]())((acc, headElem) => {
      append(acc, f(headElem))
    })

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, List[A]())((acc, headElem) => {
      if (f(headElem)) append(acc, headElem) else acc
    })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(as), List[B]())((acc, headElem) => {
      concat(f(headElem), acc)
    })

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else List())

  def sumLists(l: List[Int], l2: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(x,xs) => l2 match {
        case Nil => Nil
        case Cons(y,ys) => Cons(x+y, sumLists(xs,ys))
      }
    }

  def zipWith[A,B](l: List[A], l2: List[B])(f: (A,B) => A): List[A] =
    (l, l2) match {
      case (Nil, _) => Nil
      case (_, Nil)=> Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
    }

  def startsWith[A](l: List[A], l2: List[A]): Boolean = (l, l2) match {
    case (_, Nil) => true
    case (Cons(x,xs), Cons(y,ys)) if x == y => startsWith(xs, ys)
    case (_,_) => false
  }

  def hasSubsequence[A](mainList: List[A], subList: List[A]): Boolean =  mainList match {
    case Nil => subList == Nil
    case _ if startsWith(mainList, subList) => true
    case Cons(x,xs) => hasSubsequence(xs, subList)
  }


}





