package laziness
import Stream._

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def head: A = this match {
    case Empty => throw new Exception("the stream is empty")
    case Cons(h,t) => h()
  }

  def tail: Stream[A] = this match {
    case Empty => throw new Exception("the stream is empty")
    case Cons(h,t) => t()
  }

  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h,t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Stream()
    else Cons(() => head, () => tail.take(n-1))
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else tail.drop(n-1)
  }

  final def drop2(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop2(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Cons(() => h(), () => t().takeWhile(p))
    case _ => Stream()
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) =>
      if (p(a)) Cons(() => a,() => b)
      else b)

  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((a,_) => Option(a))

  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Stream())((a,b) => Cons(() => f(a), () => b))
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream())((a,b) =>
      if (f(a)) Cons(() => a, () => b)
      else b)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((x,y) => Cons(() => x, () => y))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Stream())((x,y) => f(x).append(y))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this,n){
      case (Cons(h,t),1) => Some(h(), (empty, 0))
      case (Cons(h,t),_) => Some(h(), (t(), n-1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t().takeWhileViaUnfold(p))
      case _ => None
    }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s)){
      case (Cons(a,b), Cons(x,y)) => Some(f(a(),x()), (b(),y()))
      case _ => None
    }

  def zipWithAll[B,C](s: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold(this, s){
      case (Cons(a,b),Cons(x,y)) => Some(f(Some(a()), Some(x())), (b(),y()))
      case (Cons(a,b),Empty) => Some(f(Some(a()), None), (b(), Empty))
      case (Empty, Cons(x,y)) => Some(f(None, Some(x())), (Empty, y()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def checkHeadOrUseTail[B](headTuple: (Option[B],Option[B]), tail: => Boolean): Boolean =
    headTuple match {
      case (Some(a), Some(b)) if a != b => false
      case (None, Some(b)) => false
      case (Some(a), None) => true
      case (None, None) => true
      case _ => tail
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).foldRight(true)(checkHeadOrUseTail)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop2(1))
    }.append(Stream(Empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

//  def scanRight[B >: A](n: B)(f: (A, B) => B): Stream[B] =
//    unfold(this){
//      case Cons(h,t) => Some(f(h(),n), t().scanRight(n)(f))
//      case Empty => None
//    }

  def scanRight[B](n: B)(f: (A, B) => B): Stream[B] = {

    foldRight[Stream[B]](Stream(n))((a,b) => {
      lazy val cachedTail: Stream[B] = b
//      cachedTail.foldRight[Stream[B]](Empty)((x,y) => Cons(() => f(a,x), () => cachedTail))
      cachedTail match {
        case Empty => Empty
        case Cons(h,t) => Cons(() => f(a,h()), () => cachedTail)
      }
    })
//    this match {
//      case Cons(h,t) => {
//        val tail: Stream[B] = t().scanRight(n)(f)
//        val head: B = f(h(), tail.headOption.getOrElse(n))
//
//
//        println(h())
//
//        Cons(() => head, () => tail)
//      }
//      case _ => Stream(n)
//    }
  }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(start: Int, acc: Int): Stream[Int] =
      cons(start, loop(acc, start + acc))
    loop(0, 1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case Some((x,y)) => cons(x,unfold(y)(f))
      case _ => Empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold(0,1)(x => Some(x._1, (x._2, x._1 + x._2)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def onesViaUnfold(a: Int): Stream[Int] =
    unfold(a)(_ => Some(a, a))
}
