package errorHandling

import org.scalatest.{Matchers, WordSpec}

class EitherListSpec extends WordSpec with Matchers {

  "mkPerson" should {
    "make a new Person if both the name and age are given values" in {
      EitherList.mkPerson("Bob", 18) shouldBe RightList(Person(Name("Bob"), Age(18)))
    }

    "fail to make a person and return an either name exception if the name is not supplied" in {
      EitherList.mkPerson("", 18) shouldBe LeftList(List("Name is empty."))
    }

    "fail to make a person and return an either age exception if the age is below zero" in {
      EitherList.mkPerson("Bob", -1) shouldBe LeftList(List("Age is out of range."))
    }

    "fail to make a person with an either list of errors if both name and age fail" in {
      EitherList.mkPerson("", -1) shouldBe LeftList(List("Name is empty.", "Age is out of range."))
    }
  }

  "orElse" should {
    "return the first either if it is not an error" in {
      RightList(1).orElse(RightList(2)) shouldBe RightList(1)
    }

    "return the second either if the first either fails" in {
      LeftList(List("error", "banana")).orElse(RightList(2)) shouldBe RightList(2)
    }
  }

  "sequence" should {
    "return a list of one value if the list is an either with a value" in {
      EitherList.sequence(List(RightList(1))) shouldBe RightList(List(1))
    }

    "combine a list of eithers into one either of a list of values, if there are no exceptions" in {
      EitherList.sequence(List(RightList(1), RightList(2), RightList(3))) shouldBe RightList(List(1,2,3))
    }

    "return an either of exception if any of the values in the list of eithers fails" in {
      EitherList.sequence(List(LeftList(List("error")), LeftList(List("error2")))) shouldBe LeftList(List("error", "error2"))
    }
  }

  "traverse" should {
    "convert a list of one element" in {
      EitherList.traverse(List(1))(a => RightList(a + 1)) shouldBe RightList(List(2))
    }

    "convert a list of elements to an either list of elements" in {
      EitherList.traverse(List(1,2,3))(a => RightList(a + 1)) shouldBe RightList(List(2,3,4))
    }

    "return an either of an exception if any value is an exception" in {
      EitherList.traverse(List(1,2,3))(a => if (a % 2 == 0) RightList(a) else LeftList(List("error"))) shouldBe LeftList(List("error", "error"))
    }
  }
}
