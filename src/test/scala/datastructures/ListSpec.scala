package datastructures

import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {

  "tail" should {
    "return the tail of a list" in {
      List.tail(List(1,2,3,4)) shouldBe List(2,3,4)
    }
  }

  "setHead" should {
    "change the list's head to specified element" in {
      List.setHead(5, List(1,2,3,4)) shouldBe List(5,2,3,4)
    }
  }

  "drop" should {
    "remove first n elements of list" in {
      List.drop(List(1,2,3,4), 2) shouldBe List(3,4)
    }
    "return the original list is 0 elements passed" in {
      List.drop(List(1,2,3,4), 0) shouldBe List(1,2,3,4)
    }
  }

  "dropWhile" should {
    "remove elements that satisfy provided predicate" in {
      List.dropWhile(List(1,2,3,4), (a: Int) => a < 3) shouldBe List(3,4)
    }
  }

  "init" should {
    "return the first elements of a list omitting the last" in {
      List.init(List(1,2,3,4)) shouldBe List(1,2,3)
    }

    "return an empty list for a list of one element" in {
      List.init(List(1)) shouldBe List()
    }

    "throw an exception if an empty list if passed" in {
      intercept[Exception] {
        List.init(List())
      }
    }
  }

  "length" should {
    "return 1 for a list with 1 element" in {
      List.length(List(1)) shouldBe 1
    }

    "return 3 for a list with 3 elements" in {
      List.length(List("1","2","3")) shouldBe 3
    }
  }

  "foldLeft" should {
    "apply a sum function to each element in list" in {
      List.foldLeft(List(1,2,3), 0)(_+_) shouldBe 6
    }

    "add each element in list to 20" in {
      List.foldLeft(List(1,2,3), 20)(_+_) shouldBe 26
    }

    "apply a multiply function to each element" in {
      List.foldLeft(List(1,2,3), 1)(_*_) shouldBe 6
    }
  }

  "sumLeft" should {
    "sum elements of a list using foldLeft" in {
      List.sumLeft(List(1,2,3)) shouldBe 6
    }
  }

  "productLeft" should {
    "multiply elements of a list together using foldLeft" in {
      List.productLeft(List(2,3,4)) shouldBe 24
    }
  }

  "lengthLeft" should {
    "count the number of elements in a list using foldleft" in {
      List.lengthLeft(List(1,2,3,4)) shouldBe 4
    }

    "count elements in a list of strings" in {
      List.lengthLeft(List("hello", "there")) shouldBe 2
    }
  }

  "reverse" should {
    "return a list with elements displayed in reverse order" in {
      List.reverse(List(1,2,3)) shouldBe List(3,2,1)
    }
  }

  "foldLeftViaFoldRight" should {
    "perform the foldLeft function by calling foldRight" in {
      List.foldLeftViaFoldRight(List(1,2,3), 1.0)(_/_) shouldBe 0.16666666666666666
    }
  }

  "foldRightViaFoldLeft" should {
    "perform the foldLeft function by calling foldRight" in {
      List.foldRightViaFoldLeft(List(1,2,3), 1.0)(_/_) shouldBe 1.5
    }
  }

  "append" should {
    "add an element to the end of a list" in {
      List.append(List(1,2,3), 4) shouldBe List(1,2,3,4)
    }
  }

  "concat" should {
    "concatenate two lists" in {
      List.concat(List(1,2), List(3,4)) shouldBe List(1,2,3,4)
    }

    "concat2 merges a list of lists" in {
      List.concat2(List(List(1,2), List(3,4))) shouldBe List(1,2,3,4)
    }
  }

  "addOne" should {
    "add 1 to each integer in a list" in {
      List.addOne(List(1,2,3)) shouldBe List(2,3,4)
    }
  }

  "doubleToString" should {
    "turn each double in a list to a string" in {
      List.doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
    }
  }

  "map" should {
    "generalize modifying elements for addOne" in {
      List.map(List(1,2,3))(_ + 1) shouldBe List(2,3,4)
    }
    "generalize modifying elements for doubleToString" in {
      List.map(List(1.0, 2.0, 3.0))(_.toString) shouldBe List("1.0", "2.0", "3.0")
    }
  }

  "filter" should {
    "remove elements from a list unless they satisfy a predicate" in {
      List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) shouldBe List(2,4,6)
    }
  }

  "flatMap" should {
    "build a new collection by applying a function to all elements of a list and using the elements of the resulting collections" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
    }
  }

  "filterViaFlatMap" should {
    "remove odd numbers from a list" in {
      List.filterViaFlatMap(List(1,2,3,4,5,6))(_ % 2 == 0) shouldBe List(2,4,6)
    }

    "remove words longer than 3 letters" in {
      List.filterViaFlatMap(List("hey", "kettle", "cat", "banana", "if", "look"))(_.length <= 3) shouldBe List("hey", "cat", "if")
    }
  }

  "sumLists" should {
    "add corresponding elements of a list together" in {
      List.sumLists(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
    }

    "drop elements from the first list that do not have a counterpart in the second" in {
      List.sumLists(List(1,2,3), List(4,5)) shouldBe List(5,7)
    }

    "drop elements from the second list that do not have a counterpart in the first" in {
      List.sumLists(List(1,2), List(4,5,6)) shouldBe List(5,7)
    }
  }

  "zipWith" should {
    "add corresponding elements of a list together" in {
      List.zipWith(List(1,2,3), List(4,5,6))(_ + _) shouldBe List(5,7,9)
    }

    "drop elements from the first list that do not have a counterpart in the second" in {
      List.zipWith(List(1,2,3), List(4,5))(_ + _) shouldBe List(5,7)
    }

    "drop elements from the second list that do not have a counterpart in the first" in {
      List.zipWith(List(1,2), List(4,5,6))(_ + _) shouldBe List(5,7)
    }

    "multiply corresponding elements of a list together" in {
      List.zipWith(List(1,2,3), List(4,5,6))(_ * _) shouldBe List(4,10,18)
    }
  }

  "hasSubsequence" should {
    "return true if a list exists within another list" in {
      List.hasSubsequence(List(1,2,3,4), List(1,2)) shouldBe true
    }

    "return true if the checking an empty list exists inside another list" in {
      List.hasSubsequence(List(1,2,3,4), List()) shouldBe true
    }

    "return false if a list does not exist within another list" in {
      List.hasSubsequence(List(1,2,3,4), List(1,3)) shouldBe false
    }

    "return false if the first list is empty" in {
      List.hasSubsequence(List(), List(1,3)) shouldBe false
    }
  }

}
