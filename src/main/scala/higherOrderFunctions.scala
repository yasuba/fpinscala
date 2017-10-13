import scala.annotation.tailrec

object higherOrderFunctions {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, start: Int, acc: Int): Int = {
      if (n == 0) start
      else if (n == 1) acc
      else loop(n-1, acc, start + acc)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b:B) => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def greetCurried(greeting: String): String => String = {
    (name: String) => s"$greeting, $name"
  }

  def greetingName(greeting: String, name: String): String = s"$greeting, $name"

  val curriedGreetingName: String => (String => String) = curry(greetingName _)

  def greetCurried2(greeting: String): String => String = curriedGreetingName(greeting)

  val uncurriedCurriedGreetingName:(String, String) => String = uncurry(curriedGreetingName)
}
