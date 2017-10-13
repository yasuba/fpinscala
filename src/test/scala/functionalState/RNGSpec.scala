package functionalState

import org.scalatest.{WordSpec, Matchers}
import RNG._

class RNGSpec extends WordSpec with Matchers {


  val RNG = new RNG {
    override def nextInt: (Int, RNG) = (-1, this)
  }

  def rng(randomNumber: Int, nextRNG: RNG = RNG) = new RNG {
    override def toString():String = s"RNG(${randomNumber.toString})"
    override def nextInt: (Int, RNG) = (randomNumber, nextRNG)
  }

  "nonNegativeInt" should {
    "return seeded random number if positive and less than MaxValue" in {
      assert(nonNegativeInt(rng(-5, rng(5)))._1 >= 0)
      assert(nonNegativeInt(rng(Int.MaxValue -1))._1 <= Int.MaxValue)
    }

    "work when passed MinValue" in {
      assert(nonNegativeInt(rng(Int.MinValue))._1 >= 0)
    }
  }

  "double" should {
    "generate a random double" in {
      assert(double(rng(0, rng(0)))._1.getClass.toString == "double")
    }
    "generate a random double less than 1" in {
      assert(double(rng(6, rng(3)))._1 < 1)
    }
    "generate a random double greater than or equal to 0.0" in {
      assert(double(rng(-2, rng(10)))._1 >= 0.0)
    }

    "never generate 1" in {
      assert(double(rng(Int.MaxValue, rng(0)))._1 != 1.0)
    }
  }

  "intDouble" should {
    "return a randomly generated int and double tuple" in {
      assert(intDouble(rng(1, rng(1)))._1._1.getClass.toString == "int")
      assert(intDouble(rng(3, rng(3)))._1._2.getClass.toString == "double")
    }

    "return a random tuple with an int that is always positive" in {
      assert(intDouble(rng(-5, rng(5)))._1._1 >= 0)
    }

    "return a random tuple with a double that is less than 1" in {
      assert(intDouble(rng(500, rng(1)))._1._2 < 1.0)
    }
  }

  "doubleInt" should {
    "return a randomly generated int and double tuple" in {
      assert(doubleInt(rng(1, rng(1)))._1._1.getClass.toString == "double")
      assert(doubleInt(rng(3, rng(3)))._1._2.getClass.toString == "int")
    }

    "return a random tuple with an int that is always positive" in {
      assert(doubleInt(rng(-5, rng(5)))._1._2 >= 0)
    }

    "return a random tuple with a double that is less than 1" in {
      assert(doubleInt(rng(500, rng(1)))._1._1 < 1.0)
    }
  }

  "double3" should {
    "return a randomly generated double 3-tuple" in {
      assert(double3(rng(1, rng(1)))._1._1.getClass.toString == "double")
      assert(double3(rng(3, rng(3)))._1._2.getClass.toString == "double")
      assert(double3(rng(5, rng(3)))._1._3.getClass.toString == "double")
    }

    "return a random 3-tuple with doubles that are always positive" in {
      assert(double3(rng(-5, rng(5)))._1._1 >= 0)
    }

    "return a random 3-tuple with doubles that are less than 1" in {
      assert(double3(rng(500, rng(1)))._1._2 < 1.0)
    }

    "never return a double greater than 1.0" in {
      assert(double3(rng(500, rng(1)))._1._3 < 1.0)
    }
  }

  "ints" should {
    "return a list of a specified number of random ints" in {
      assert(ints(2)(rng(3, rng(8)))._1.length === 3)
    }

    "return random numbers in the list" in {
      val nextRNG = rng(13)
      val seed: RNG = rng(6, rng(4, rng(28, nextRNG)))
      val expectedOutput = List(6,4,28)
      assert(ints(2)(seed)._1 == expectedOutput)
      assert(ints(2)(seed)._2 == nextRNG)
    }
  }

  "nonNegativeEven" should {
    val x: Rand[Int] = nonNegativeEven
    "generate an Int that's divisible by two" in {
      assert(x(rng(3))._1 == 2)
    }

    "generate an Int greater than or equal to zero" in {
      assert(x(rng(-2))._1 >= 0)
    }
  }

  "mapDouble" should {
    val x: Rand[Double] = mapDouble
    "generate a random double less than 1" in {
      assert(x(rng(5))._1 < 1.0)
    }

    "generate a random double greater than or equal to 0.0" in {
      assert(x(rng(-2))._1 >= 0.0)
    }

    "never generate 1" in {
      assert(x(rng(Int.MaxValue))._1 != 1.0)
    }
  }

//  "map2" should {
//    val d: Rand[Double] = mapDouble
//    val i: Rand[Int] = nonNegativeInt
//    "return a new action combining the two Rand arguments" in {
//      map2(i, d)((i, d) => ) shouldBe 3
//    }
//  }
}
