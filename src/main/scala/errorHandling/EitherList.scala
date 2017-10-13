package errorHandling

trait EitherList[+E, +A] {

  def map[B](f: A => B): EitherList[E, B] =
    this match {
      case LeftList(e) => LeftList(e)
      case RightList(a) => RightList(f(a))
    }

  def mapListOfExceptions[EE >: E,B,C](b: EitherList[EE,B])(f: (A,B) => C): EitherList[EE, C] =
    (this, b) match {
      case (LeftList(e), LeftList(e2)) => LeftList(e ::: e2)
      case (RightList(a), RightList(b)) => RightList(f(a,b))
      case (LeftList(e), _) => LeftList(e)
      case (_, LeftList(e)) => LeftList(e)
    }

  def orElse[EE >: E,B >: A](b: => EitherList[EE,B]): EitherList[EE,B] =
    this match {
      case LeftList(e) => b
      case RightList(a) => RightList(a)
    }

}

object EitherList {

  def mkName(name: String): EitherList[String, Name] =
    if (name == "" || name == null) LeftList(List("Name is empty."))
    else RightList(new Name(name))


  def mkAge(age: Int): EitherList[String, Age] =
    if (age < 0) LeftList(List("Age is out of range."))
    else RightList(new Age(age))

  def mkPerson(name: String, age: Int): EitherList[String, Person] =
    mkName(name).mapListOfExceptions(mkAge(age))(Person(_,_))


  def sequence[E,A](es: List[EitherList[E,A]]): EitherList[E, List[A]] =
    es match {
      case LeftList(e) :: Nil => LeftList(e)
      case LeftList(e) :: t => sequence(t) match {case LeftList(e1) => LeftList(e ::: e1) case RightList(a) => LeftList(e)} // thrown away the e
      case RightList(a) :: Nil => RightList(List(a))
      case RightList(a) :: t => sequence(t).map(t1 => a :: t1)
    }

  def traverse[E,A,B](as: List[A])(f: A => EitherList[E,B]): EitherList[E,List[B]] = {
    as match {
      case h :: t => f(h).mapListOfExceptions(traverse(t)(f))(_ :: _)
      case Nil => RightList(List())
    }
  }

  def sequenceViaTraverse[E,A](es: List[EitherList[E,A]]): EitherList[E, List[A]] = {
    traverse[E,EitherList[E,A],A](es)(x => x)
  }
}

case class LeftList[+E](value: List[E]) extends EitherList[E, Nothing]
case class RightList[+A](value: A) extends EitherList[Nothing, A]
