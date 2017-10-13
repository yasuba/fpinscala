package errorHandling

sealed trait Optional[+A] {
  def map[B](f: A => B): Optional[B] = {
    this match {
      case NoneType => NoneType
      case SomeType(x) => SomeType(f(x))
    }
  }

  def flatMap[B](f: A => Optional[B]): Optional[B] = {
    map(f).getOrElse(NoneType)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case NoneType => default
      case SomeType(x) => x
    }
  }

  def orElse[B >: A](ob: => Optional[B]): Optional[B] = {
    map(x => SomeType(x)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Optional[A] = {
    flatMap(x => if (f(x)) SomeType(x) else NoneType)
  }

}

case class SomeType[+A](get: A) extends Optional[A]
case object NoneType extends Optional[Nothing]

object Optional {

  def mean(xs: Seq[Double]): Optional[Double] = {
    if (xs.isEmpty) NoneType
    else SomeType(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Optional[Double] = {
    mean(xs).flatMap(meanOfSeq => mean(xs.map(x => math.pow(x - meanOfSeq, 2))))
  }

  def map2[A,B,C](a: Optional[A], b: Optional[B])(f: (A,B) => C): Optional[C] = {
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))
  }

  def sequence[A](a: List[Optional[A]]): Optional[List[A]] = {
    a match {
      case h :: t => h.flatMap(h2 => sequence(t).map(t2 => h2 :: t2))
      case Nil => SomeType(Nil)
      case _ => NoneType
    }
  }

  def traverse[A,B](a: List[A])(f: A => Optional[B]): Optional[List[B]] = {
    a match {
      case h :: t => f(h).flatMap(h1 => traverse(t)(f).map(t1 => h1 :: t1))
      case Nil => SomeType(Nil)
    }
  }

  def sequenceViaTraverse[A](a: List[Optional[A]]): Optional[List[A]] = {
    traverse(a)(a1 => a1)
  }
}

