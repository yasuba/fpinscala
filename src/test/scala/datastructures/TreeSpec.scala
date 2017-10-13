package datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "size" should {
    "count one leaf" in {
      Tree.size(Leaf(1)) shouldBe 1
    }

    "count the number of leaves on a branch" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    }

    "count leaves and branches on a tree" in {
      Tree.size(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(6), Leaf(7)))) shouldBe 7
  }

    "count several branches with leaves on a tree" in {
      Tree.size(Branch(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(7), Leaf(8))),Branch(Branch(Leaf(11), Leaf(12)), Branch(Leaf(14), Leaf(15))))) shouldBe 15
    }

    "count an uneven tree" in {
      Tree.size(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 5
    }
  }

  "maximum" should {
    "return the leaf value if there is only one leaf" in {
      Tree.maximum(Leaf(1)) shouldBe 1
    }

    "return the maximum element in a Tree with one branch" in {
      Tree.maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
    }

    "return the maximum element in a tree with more than one branch" in {
      Tree.maximum(Branch(Branch(Branch(Leaf(7), Leaf(15)), Branch(Leaf(17), Leaf(30))),Branch(Branch(Leaf(1), Leaf(22)), Branch(Leaf(1), Leaf(15))))) shouldBe 30
    }

    "return the maximum element in an uneven tree" in {
      Tree.maximum(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 5
    }
  }

  "depth" should {
    "return path length 0 when there is only 1 leaf in a tree" in {
      Tree.depth(Leaf(1)) shouldBe 0
    }

    "return path length 1 when there are 2 leaves in a tree" in {
      Tree.depth(Branch(Leaf(1), Leaf(2))) shouldBe 1
    }

    "return the path length from root to any leaf in a tree" in {
      Tree.depth(Branch(Branch(Branch(Leaf(7), Leaf(15)), Branch(Leaf(17), Leaf(30))),Branch(Branch(Leaf(1), Leaf(22)), Branch(Leaf(1), Leaf(15))))) shouldBe 3
    }
  }

  "map" should {
    "modify each element in a tree with a given addition function" in {
      Tree.map(Branch(Leaf(1), Leaf(2)))(_+1) shouldBe Branch(Leaf(2), Leaf(3))
    }

    "modify each element in an uneven tree with a function" in {
      Tree.map(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))(_.toString) shouldBe Branch(Branch(Leaf("3"), Leaf("4")), Leaf("5"))
    }
  }

  "fold" should {
    "generalize the size function" in {
      Tree.size2(Branch(Leaf(1), Leaf(2))) shouldBe 3
    }

    "generalize size to count one leaf" in {
      Tree.size2(Leaf(1)) shouldBe 1
    }

    "generalize counting an uneven tree" in {
      Tree.size2(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 5
    }

    "generalize the maximum function" in {
      Tree.maximum2(Branch(Leaf(1), Leaf(2))) shouldBe 2
    }

    "generalize returning path length 0 when there is only 1 leaf in a tree" in {
      Tree.depth2(Leaf(1)) shouldBe 0
    }

    "generalize returning path length 1 when there are 2 leaves in a tree" in {
      Tree.depth2(Branch(Leaf(1), Leaf(2))) shouldBe 1
    }

    "generalize returning the path length from root to any leaf in a tree" in {
      Tree.depth2(Branch(Branch(Branch(Leaf(7), Leaf(15)), Branch(Leaf(17), Leaf(30))),Branch(Branch(Leaf(1), Leaf(22)), Branch(Leaf(1), Leaf(15))))) shouldBe 3
    }

    "modify each element in a tree with a given addition function using fold" in {
      Tree.map2(Branch(Leaf(1), Leaf(2)))(_+1) shouldBe Branch(Leaf(2), Leaf(3))
    }

    "modify each element in an uneven tree with a function using fold" in {
      Tree.map2(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))(_.toString) shouldBe Branch(Branch(Leaf("3"), Leaf("4")), Leaf("5"))
    }

  }
}
