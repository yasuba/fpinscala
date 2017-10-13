package errorHandling

import org.scalatest.{Matchers, WordSpec}


class OptionSpec extends WordSpec with Matchers {

  "map" should {
    "apply the given function to option if some else return none" in {
      SomeType(1).map(_ + 2) shouldBe SomeType(3)
    }

    "return None if the option map is called on is None" in {
      NoneType.map(_.toString) shouldBe NoneType
    }
  }

  "flatMap" should {
    "build a new option by applying a function to an option" in {
      SomeType("hi").flatMap((x:String) => SomeType(x.toUpperCase)) shouldBe SomeType("HI")
    }
  }

  "getOrElse" should {
    "return the value if Some" in {
      SomeType(1).getOrElse("nothing") shouldBe 1
    }

    "return the supplied default if called on None" in {
      NoneType.getOrElse("nothing") shouldBe "nothing"
    }
  }

  "orElse" should {
    "return the first Option if it's defined" in {
      SomeType(1).orElse(SomeType(2)) shouldBe SomeType(1)
    }

    "return the second option if the first option is not defined" in {
      NoneType.orElse(SomeType(2)) shouldBe SomeType(2)
    }
  }

  "filter" should {
    "return a value that satisfies a given predicate" in {
      SomeType(1).filter(_ == 1) shouldBe SomeType(1)
    }

    "not return a value when predicate is not satisfied" in {
      SomeType(2).filter(_ == 1) shouldBe NoneType
    }

    "not return a value when called on None" in {
      NoneType.filter(_ == 1) shouldBe NoneType
    }
  }

  "variance" should {
    "work out the variance of a sequence of doubles" in {
      Optional.variance(Seq(1.0,2.0,3.0)) shouldBe SomeType(0.6666666666666666)
    }

    "return None if called on an empty Seq" in {
      Optional.variance(Seq()) shouldBe NoneType
    }
  }

  "map2" should {
    "combine two option values using a binary function" in {
      Optional.map2(SomeType(1), SomeType(2))(_ + _) shouldBe SomeType(3)
    }

    "return None if the first value is none" in {
      Optional.map2(NoneType, SomeType(2))((a:Int,b:Int) => a + b) shouldBe NoneType
    }

    "return None if the second value is none" in {
      Optional.map2(SomeType(1), NoneType)((a:Int,b:Int) => a + b) shouldBe NoneType
    }

    "return None if both values are none" in {
      Optional.map2(NoneType, NoneType)((a:Int,b:Int) => a + b) shouldBe NoneType
    }
  }

  "sequence" should {
    "combine a list of options into one option of a list of values" in {
      Optional.sequence(List(SomeType(1), SomeType(2), SomeType(3))) shouldBe SomeType(List(1,2,3))
    }

    "return none if any of the values in the list of optionals is none" in {
      Optional.sequence(List(SomeType(1), SomeType(2), NoneType)) shouldBe NoneType
    }

    "return none if the list is empty" in {
      Optional.sequence(List()) shouldBe SomeType(List())
    }
  }

  "traverse" should {
    "convert a list of elements to an optional list of elements" in {
      Optional.traverse(List(1,2,3))(a => SomeType(a)) shouldBe SomeType(List(1,2,3))
    }

    "convert a list of one element" in {
      Optional.traverse(List(1))(a => SomeType(a)) shouldBe SomeType(List(1))
    }
  }

  "sequenceViaTraverse" should {
    "use the traverse method to change a list of optionals to an option of a list" in {
      Optional.sequenceViaTraverse(List(SomeType(1), SomeType(2), SomeType(3))) shouldBe SomeType(List(1,2,3))
    }
    "return none if any of the values in the list of optionals is none" in {
      Optional.sequenceViaTraverse(List(SomeType(1), SomeType(2), NoneType)) shouldBe NoneType
    }

    "return none if the list is empty" in {
      Optional.sequenceViaTraverse(List()) shouldBe SomeType(List())
    }
  }
}
