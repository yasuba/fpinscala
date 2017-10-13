package laziness

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "toList" should {
    "extract elements of a Stream into a list" in {
      Stream(1,2,3).toList shouldBe List(1,2,3)
    }

    "return an empty list if the Stream is Empty" in {
      Stream().toList shouldBe List()
    }

    "return different types of elements in a Stream" in {
      Stream("a","b","c").toList shouldBe List("a","b","c")
    }
  }

  "take" should {
    "returning the first n elements of a Stream" in {
      Stream(1,2,3).take(1).toList shouldBe List(1)
    }

    "return no elements if 0 is passed to it" in {
      Stream(1,2,3).take(0).toList shouldBe List()
    }
  }

  "drop" should {
    "returning the last n elements of a Stream" in {
      Stream(1,2,3).drop(1).toList shouldBe List(2,3)
    }

    "return all elements if 0 is passed to it" in {
      Stream(1,2,3).drop(0).toList shouldBe List(1,2,3)
    }
  }

  "drop2" should {
    "return no elements if the number required is greater than all the elements" in {
      Stream(1,2,3).drop2(4).toList shouldBe List()
    }
  }

  "takeWhile" should {
    "returning all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3).takeWhile(_ < 3).toList shouldBe List(1,2)
    }

    "return an empty stream if no element matches the predicate" in {
      Stream(1,2,3).takeWhile(_ == 4).toList shouldBe List()
    }

    "return an empty stream if the stream the method is called on is empty" in {
      Stream().takeWhile(_ == 1).toList shouldBe List()
    }
  }

  "forAll" should {
    "return true when all elements in stream match a predicate" in {
      Stream(1,2,3).forAll(_ < 4) shouldBe true
    }

    "return false when not all the elements match a predicate" in {
      Stream(1,2,3).forAll(_ < 3) shouldBe false
    }
  }

  "takeWhileViaFoldRight" should {
    "returning all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1,2)
    }

    "return an empty stream if no element matches the predicate" in {
      Stream(1,2,3).takeWhileViaFoldRight(_ == 4).toList shouldBe List()
    }

    "return an empty stream if the stream the method is called on is empty" in {
      Stream().takeWhileViaFoldRight(_ == 1).toList shouldBe List()
    }
  }

  "headOptionViaFoldRight" should {
    "optionally return the head of the stream" in {
      Stream(1,2).headOptionViaFoldRight shouldBe Some(1)
    }

    "return none for an empty stream" in {
      Stream().headOptionViaFoldRight shouldBe None
    }
  }

  "map" should {
    "perform a function on each element of a stream" in {
      Stream(1,2,3).map((x: Int) => x + 1).toList shouldBe List(2,3,4)
    }

    "return an empty stream if called on an empty stream" in {
      Stream().map((x: Int) => x + 1).toList shouldBe List()
    }
  }

  "filter" should {
    "remove elements from a list unless they satisfy a predicate" in {
      Stream(1,2,3).filter(_ > 2).toList shouldBe List(3)
    }

    "return an empty stream if called on an empty stream" in {
      Stream().filter(_ == 1).toList shouldBe List()
    }
  }

  "append" should {
    "add an element to the end of a list" in {
      Stream(1,2,3).append(Stream(4)).toList shouldBe List(1,2,3,4)
    }

    "return a stream containing only the element passed to the function" in {
      Stream().append(Stream(1)).toList shouldBe List(1)
    }
  }

  "flatMap" should {
    "build a new stream by applying a function to all elements of a stream and using the elements of the resulting collections" in {
      Stream(1,2,3).flatMap(i => Stream(i,i)).toList shouldBe List(1,1,2,2,3,3)
    }
  }

  "find" should {
    "find an element in a stream that matches a parameter" in {
      Stream(1,2,3).find(_ % 2 == 0) shouldBe Some(2)
    }
  }

  "constant" should {
    "return an infinite Stream of a given value" in {
      Stream.constant(1).take(5).toList shouldBe List(1,1,1,1,1)
    }
  }

  "from" should {
    "return an infinite incrementing Stream starting from a given value" in {
      Stream.from(1).take(5).toList shouldBe List(1,2,3,4,5)
    }
  }

  "fibs" should {
    "return an infinite incrementing Stream in the fibonacci sequence starting from a given value" in {
      Stream.fibs.take(5).toList shouldBe List(0,1,1,2,3)
    }
  }

  "unfold" should {
    "take an initial state and a function for producing both the next state and the next value in the generated stream" in {
      Stream.unfold(1)(s => if (s > 10) None else Some(s, s + 1)).toList shouldBe List(1,2,3,4,5,6,7,8,9,10)
    }
  }

  "fibsViaUnfold" should {
    "return the fibonacci sequence using the unfold function" in {
      Stream.fibsViaUnfold.takeWhile(_ < 10).toList shouldBe List(0,1,1,2,3,5,8)
    }
  }

  "fromViaUnfold" should {
    "return an incrementing stream using the unfold function" in {
      Stream.fromViaUnfold(1).takeWhile(_ < 6).toList shouldBe List(1,2,3,4,5)
    }
  }

  "onesViaUnfold" should {
    "return an infinite stream of ones using unfold" in {
      Stream.onesViaUnfold(1).take(5).toList shouldBe List(1,1,1,1,1)
    }
  }

  "mapViaUnfold" should {
    "perform a function on every element in a stream using unfold" in {
      Stream(1,2,3).mapViaUnfold(_ + 1).toList shouldBe List(2,3,4)
    }
  }

  "takeViaUnfold" should {
    "take the first n elements from an element using unfold" in {
      Stream(1,2,3).takeViaUnfold(2).toList shouldBe List(1,2)
    }
  }

  "takeWhileViaUnfold" should {
    "return all starting elements of a Stream that match the given predicate using unfold" in {
      Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList shouldBe List(1, 2)
    }

    "return an empty stream if no element matches the predicate" in {
      Stream(1,2,3).takeWhileViaUnfold(_ == 4).toList shouldBe List()
    }

    "return an empty stream if the stream the method is called on is empty" in {
      Stream().takeWhileViaUnfold(_ == 1).toList shouldBe List()
    }
  }

  "zipWith" should {
    "add corresponding elements of a list together" in {
      Stream(1,2,3).zipWith(Stream(4,5,6))(_ + _).toList shouldBe List(5,7,9)
    }

    "drop elements from the first list that do not have a counterpart in the second" in {
      Stream(1,2,3).zipWith(Stream(4,5))(_ + _).toList shouldBe List(5,7)
    }

    "drop elements from the second list that do not have a counterpart in the first" in {
      Stream(1,2).zipWith(Stream(4,5,6))(_ + _).toList shouldBe List(5,7)
    }

    "multiply corresponding elements of a list together" in {
      Stream(1,2,3).zipWith(Stream(4,5,6))(_ * _).toList shouldBe List(4,10,18)
    }
  }

  "zipAll" should {
    "combine two lists into tuples" in {
      Stream(1,2,3).zipAll(Stream(4,5,6)).toList shouldBe List((Some(1),Some(4)), (Some(2),Some(5)), (Some(3),Some(6)))
    }

    "still work if the second list is shorter than the first" in {
      Stream(1,2,3).zipAll(Stream(4,5)).toList shouldBe List((Some(1),Some(4)), (Some(2),Some(5)), (Some(3), None))
    }

    "still work if the first list is shorter than the second" in {
      Stream(1,2).zipAll(Stream(4,5,6)).toList shouldBe List((Some(1),Some(4)), (Some(2),Some(5)), (None, Some(6)))
    }
  }

  "startsWith" should {
    "check if one stream starts with the same elements as another" in {
      Stream(1,2,3).startsWith(Stream(1,2)) shouldBe true
    }

    "return false if the substream does not match the start of the main stream" in {
      Stream(1,2,3).startsWith(Stream(4,5)) shouldBe false
    }

    "return false if the substream is longer than the main stream" in {
      Stream(1,2,3).startsWith(Stream(1,2,3,4)) shouldBe false
    }

    "return true if the second Stream is empty" in {
      Stream(1,2,3).startsWith(Stream()) shouldBe true
    }

    "return true if both streams are empty" in {
      Stream().startsWith(Stream()) shouldBe true
    }

    "return false if the main stream is empty" in {
      Stream().startsWith(Stream(1)) shouldBe false
    }

    "return false if streams match at first but then diverge" in {
      Stream(1,2,3).startsWith(Stream(1,2,4)) shouldBe false
    }

    "return true when both streams are equal" in {
      Stream(1,2,3).startsWith(Stream(1,2,3)) shouldBe true
    }
  }

  "tail" should {
    "return the tail of a stream" in {
      Stream(1,2,3).tail.toList shouldBe List(2,3)
    }
  }

  "tails" should {
    "return the stream of suffixes of the input sequence" in {
      Stream(1,2,3).tails.map(s => s.toList).toList shouldBe List(List(1,2,3), List(2,3), List(3), List())
    }
  }

  "hasSubsequence" should {
    "return true if a list exists within another list" in {
      Stream(1,2,3,4).hasSubsequence(Stream(1,2)) shouldBe true
    }

    "return true if the checking an empty list exists inside another list" in {
      Stream(1,2,3,4).hasSubsequence(Stream()) shouldBe true
    }

    "return false if a list does not exist within another list" in {
      Stream(1,2,3,4).hasSubsequence(Stream(1,3)) shouldBe false
    }

    "return false if the first list is empty" in {
      Stream().hasSubsequence(Stream(1,3)) shouldBe false
    }
  }

  "scanRight" should {
    "acts like foldRight returning a stream of intermediate results" in {
      Stream(1,2,3).scanRight(0)(_+_).toList shouldBe List(6,5,3,0)
    }
  }

}
