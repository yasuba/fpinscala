import org.scalatest.{Matchers, WordSpec}

class higherOrderFunctionsSpec extends WordSpec with Matchers {

  "fibonacci" should {

    "know the Fibonacci sequence for the 1st number is 0" in {
      higherOrderFunctions.fib(0) shouldBe 0
    }

    "know the Fibonacci sequence for the 2nd number is 1" in {
      higherOrderFunctions.fib(1) shouldBe 1
    }

    "know the Fibonacci sequence for the 5th number is 5" in {
      higherOrderFunctions.fib(5) shouldBe 5
    }
  }

  "isSorted" should {

    "check an array can be sorted in ascending numerical order" in {
      higherOrderFunctions.isSorted(Array(1,2,3,4), (x: Int, y: Int) => x + 1 == y) shouldBe true
    }

    "know an array is not sorted in ascending numerical order" in {
      higherOrderFunctions.isSorted(Array(2,3,3,5), (x: Int, y: Int) => x + 1 == y) shouldBe false
    }

    "check an array is sorted in alphabetical order" in {
      higherOrderFunctions.isSorted(Array("a", "b", "c", "d"), (x: String, y: String) => x < y) shouldBe true
    }
  }

  "greetCurried" should {

    "return a personalised greeting" in {
      val greetHello = higherOrderFunctions.greetCurried("Hello")
      greetHello("Maya") shouldBe "Hello, Maya"
      greetHello("Bob") shouldBe "Hello, Bob"
    }

    "allow different greetings" in {
      higherOrderFunctions.greetCurried("Hi there")("Maya") shouldBe "Hi there, Maya"
    }


  }
}
