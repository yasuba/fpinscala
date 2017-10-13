package poker

import org.scalatest.{WordSpec, Matchers}

class CardSpec extends WordSpec with Matchers {
  val D1 = Card(1, "D")
  val D2 = Card(2, "D")
  val D3 = Card(3, "D")
  val D10 = Card(10, "D")
  val D11 = Card(11, "D")
  val D12 = Card(12, "D")
  val D13 = Card(13, "D")
  val D14 = Card(14, "D")
  val H1 = Card(1, "H")
  val H4 = Card(4, "H")
  val H5 = Card(5, "H")
  val H6 = Card(6, "H")
  val H10 = Card(10, "H")
  val H11 = Card(11, "H")
  val H14 = Card(14, "H")
  val S1 = Card(1, "S")
  val S2 = Card(2, "S")
  val S3 = Card(3, "S")
  val S4 = Card(4, "S")
  val S5 = Card(5, "S")
  val S10 = Card(10, "S")
  val C1 = Card(1, "C")
  val C2 = Card(2, "C")
  val C3 = Card(3, "C")
  val C4 = Card(4, "C")
  val C5 = Card(5, "C")
  val C6 = Card(6, "C")
  val straight = Hand(List(D1, C2, D3, S4, C5))
  val highStraight = Hand(List(D2, D3, H4, C5, C6))
  val sixHigh = Hand(List(H1, D2, S3, C5, H6))
  val onePair = Hand(List(D1, H1, S3, C5, H6))
  val royalFlush = Hand(List(D10, D11, D12, D13, D14))
  val flush = Hand(List(C1, C2, C4, C5, C6))
  val highFlush = Hand(List(D1, D3, D10, D11, D14))
  val straightFlush = Hand(List(C1, C2, C3, C4, C5))
  val fourOfAKind = Hand(List(D1, C1, S1, H1, H5))
  val threeOfAKind = Hand(List(D1, C1, S1, S2, H5))
  val threeOfAHigherKind = Hand(List(D10, H10, S10, C1, C2))
  val fullHouse = Hand(List(D1, C1, S1, C2, D2))
  val highFullHouse = Hand(List(D10, H10, S10, H4, C4))
  val lowTwoPair = Hand(List(D1, D2, C1, C2, S3))
  val tenPair = Hand(List(D10, H10, S3, C5, H6))
  val acePair = Hand(List(D14, H14, S3, C5, H6))
  val highTwoPair = Hand(List(D10, D11, H10, H11, C1))
  val mediumTwoPair = Hand(List(D2, D3, C2, C3, H10))

  "cards" should {
    "have a value" in {
      H1.value shouldBe 2
    }

    "have a suit" in {
      H1.suit shouldBe "H"
    }

  }

  "a hand" should {
    "know if it has a flush" in {
      flush.isFlush shouldBe true
    }

    "know if it has a straight flush" in {
      straightFlush.isStraightFlush shouldBe true
    }

    "know if it has a royal flush" in {
      royalFlush.isRoyalFlush shouldBe true
    }

    "know if it has four of a kind" in {
      fourOfAKind.isFourOfAKind shouldBe true
    }

    "know if it has three of a kind" in {
      threeOfAKind.isThreeOfAKind shouldBe true
    }

    "know if it has two of a kind" in {
      onePair.isOnePair shouldBe true
    }

    "know if it has a full house" in {
      fullHouse.isFullHouse shouldBe true
    }

    "know if it has a straight" in {
      straight.isStraight shouldBe true
    }

    "know if it has two pair" in {
      highTwoPair.isTwoPair shouldBe (true, 11)
    }

    "know what it has a pair of" in {
      onePair.isOnePair shouldBe(true, 1)
    }

    "know it's highest card" in {
      straight.highestCard shouldBe 5
    }

  }

  "a game" should {
    "find a hand's winning move" in {
      Game.bestHand(royalFlush) shouldBe (10, "Royal Flush")
    }

    "choose a pair of aces over a pair of tens" in {
      Game.play(acePair, tenPair) shouldBe (acePair, "One Pair")
    }

    "one pair should beat six high" in {
      Game.play(onePair, sixHigh) shouldBe (onePair, "One Pair")
    }

    "royal flush should beat full house" in {
      Game.play(fullHouse, royalFlush) shouldBe (royalFlush, "Royal Flush")
    }

    "two pair of higher value should beat two pair of lower value" in {
      Game.play(highTwoPair, lowTwoPair) shouldBe (highTwoPair, "Two Pair")
    }

    "three of a higher card wins over three of a lower kind" in {
      Game.play(threeOfAHigherKind, threeOfAKind) shouldBe (threeOfAHigherKind, "Three Of A Kind")
    }

    "a higher straight should beat a lower straight" in {
      Game.play(highStraight, straight) shouldBe (highStraight, "Straight")
    }

    "high flush beats low flush" in {
      Game.play(highFlush, flush) shouldBe (highFlush, "Flush")
    }

    "high full house beats low full house" in {
      Game.play(highFullHouse, fullHouse) shouldBe (highFullHouse, "Full House")
    }
  }
}
