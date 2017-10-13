package errorHandling

import org.scalatest.{Matchers, WordSpec}

class EitherSpec extends WordSpec with Matchers {

  "safeDiv" should {
    "what does an either look like??" in {
      Either2.safeDiv(6,2) shouldBe Right(3)
    }
  }

  "map" should {
    "return an either of a result when passed a predicate to map" in {
      Right(1).map(x => x + 1) shouldBe Right(2)
    }

    "return an either of an exception if the predicate cannot be mapped" in {
      Left("error").map(_ == 1) shouldBe Left("error")
    }
  }

  "flatMap" should {
    "build a new either by applying a function to an either" in {
      Right(1).flatMap(x => Right(x+1)) shouldBe Right(2)
    }

    "return an either of an exception if the mapping fails" in {
      Left("error").flatMap(x => Right(x)) shouldBe Left("error")
    }
  }

  "orElse" should {
    "return the first either if it is not an error" in {
      Right(1).orElse(Right(2)) shouldBe Right(1)
    }

    "return the second either if the first either fails" in {
      Left("error").orElse(Right(2)) shouldBe Right(2)
    }
  }

  "map2" should {
    "combine two either values using a binary function" in {
      Right(1).map2(Right(2))(_+_) shouldBe Right(3)
    }

    "return either of exception if the first value fails" in {
      Left("error").map2(Right(2))((a:Int,b:Int) => a + b) shouldBe Left("error")
    }

    "return either of exception if the second value fails" in {
      Right(1).map2(Left("error"))((a:Int,b:Int) => a + b) shouldBe Left("error")
    }

    "return either of exception if both values fail" in {
      Left("error").map2(Left("error"))((a:Int,b:Int) => a + b) shouldBe Left("error")
    }
  }

  "sequence" should {
    "return a list of one value if the list is an either with a value" in {
      Either2.sequence(List(Right(1))) shouldBe Right(List(1))
    }

    "combine a list of eithers into one either of a list of values, if there are no exceptions" in {
      Either2.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1,2,3))
    }

    "return an either of exception if any of the values in the list of eithers fails" in {
      Either2.sequence(List(Right(1), Left("error"))) shouldBe Left("error")
    }
  }

  "traverse" should {
    "convert a list of one element" in {
      Either2.traverse(List(1))(a => Right(a + 1)) shouldBe Right(List(2))
    }

    "convert a list of elements to an either list of elements" in {
      Either2.traverse(List(1,2,3))(a => Right(a + 1)) shouldBe Right(List(2,3,4))
    }

    "return an either of an exception if any value is an exception" in {
      Either2.traverse(List(1))(a => Right(a / 0)) shouldBe Left("error")
    }
  }

  "sequenceViaTraverse" should {
    "use the traverse method to change a list of optionals to an option of a list" in {
      Either2.sequenceViaTraverse(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1,2,3))
    }
    "return none if any of the values in the list of optionals is none" in {
      Either2.sequenceViaTraverse(List(Right(1), Left("error"))) shouldBe Left("error")
    }
  }

  "mkPerson" should {
    "make a Person if the name and age are given" in {
      Either2.mkPerson("Maya", 24) shouldBe Right(Person(Name("Maya"), Age(24)))
    }

    "return an either of an exception if there is no name" in {
      Either2.mkPerson("", -1) shouldBe Left(List("Name is empty.", "Age is out of range."))
    }
  }
}
