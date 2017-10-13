package errorHandling

trait Either2[+E, +A] {

  def map[B](f: A => B): Either2[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E,B](f: A => Either2[EE,B]): Either2[EE,B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E,B >: A](b: => Either2[EE,B]): Either2[EE,B] =
    this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E,B,C](b: Either2[EE,B])(f: (A,B) => C): Either2[EE,C] =
    flatMap(a => b.map(b1 => f(a, b1)))

  def mapListExceptions[EE >: E,B,C](b: Either2[EE,B])(f: (A,B) => C) =
    (this, b) match {
      case (Left(e), Left(ee)) => Left(List(e,ee))
      case _ => flatMap(a => b.map(b1 => f(a, b1)))
    }
}

case class Left[+E](value: E) extends Either2[E, Nothing]
case class Right[+A](value: A) extends Either2[Nothing, A]

object Either2 {
  def mean(xs: IndexedSeq[Double]): Either2[String, Double] =
    if (xs.isEmpty) Left("mean of empty list") else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either2[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either2[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E,A](es: List[Either2[E,A]]): Either2[E, List[A]] =
    es match {
      case Left(e) :: _ => Left(e)
      case Right(a) :: Nil => Right(List(a))
      case Right(a) :: t => sequence(t).map(t1 => a :: t1)
    }

  def traverse[E,A,B](as: List[A])(f: A => Either2[E,B]): Either2[E,List[B]] =
    as match {
      case h :: t => f(h).flatMap(h1 => traverse(t)(f).map(b => h1 :: b))
      case Nil => Right(List())
    }

  def sequenceViaTraverse[E,A](es: List[Either2[E,A]]): Either2[E, List[A]] =
    traverse(es)(x => x)

  def mkName(name: String): Either2[String, Name] = {
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either2[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int) =
    mkName(name).mapListExceptions(mkAge(age))(Person(_, _))
}

case class Person(name: Name, age: Age)
case class Name(val value: String)
case class Age(val value: Int)





