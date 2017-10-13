package poker

import scala.util.Random

case class Card(value: Int, suit: String)
case class Hand(cards: List[Card]) {

  def isRoyalFlush: Boolean =
    cards.map(_.value).sum == 60 && isFlush._1

  def isStraightFlush: Boolean =
    isFlush._1 && isStraight._1

  def isFlush: (Boolean, Int) =
    (cards.map(_.suit).toSet.size == 1, cards.map(_.value).sum)

  val groupDuplicateValues = cards.map(_.value).groupBy(identity).values

  def isFourOfAKind: Boolean = {
    groupDuplicateValues.exists(_.size == 4)
  }

  def isThreeOfAKind: (Boolean, Int) = {
    val dupes = groupDuplicateValues.filter(_.size == 3)
    if (dupes.nonEmpty)
      (dupes.size == 1, dupes.flatten.head)
    else
      (false, 0)

  }

  def isOnePair: (Boolean, Int) = {
    val dupes = groupDuplicateValues.filter(_.size == 2)
    if (dupes.nonEmpty)
      (dupes.size == 1, dupes.flatten.head)
    else
      (false, 0)
  }

  def isTwoPair: (Boolean, Int) = {
    val duplicates = groupDuplicateValues.filter(_.size == 2)
    (duplicates.size == 2, duplicates.flatten.head)
  }

  def highestCard: Int =
    cards.map(_.value).max


  def isFullHouse: (Boolean, Int) =
    (isThreeOfAKind._1 && isOnePair._1, isThreeOfAKind._2 + isOnePair._2)

  def isStraight: (Boolean, Int) = {
    if (cards.map(_.value).distinct.size < 5) (false, 0)
    else (cards.map(_.value).max - cards.map(_.value).min == 4, cards.map(_.value).sum)
  }

}

object Game {

  type WinningMove = String

  lazy val deck: List[Card] = List(
  Card(1, "D"),
  Card(2, "D"),
  Card(3, "D"),
  Card(4, "D"),
  Card(5, "D"),
  Card(6, "D"),
  Card(7, "D"),
  Card(8, "D"),
  Card(9, "D"),
   Card(10, "D"),
   Card(11, "D"),
   Card(12, "D"),
   Card(13, "D"),
   Card(14, "D"),
  Card(1, "H"),
  Card(2, "H"),
  Card(3, "H"),
  Card(4, "H"),
  Card(5, "H"),
  Card(6, "H"),
  Card(7, "H"),
  Card(8, "H"),
  Card(9, "H"),
   Card(10, "H"),
   Card(11, "H"),
   Card(12, "H"),
   Card(13, "H"),
   Card(14, "H"),
  Card(1, "S"),
  Card(2, "S"),
  Card(3, "S"),
  Card(4, "S"),
  Card(5, "S"),
  Card(6, "S"),
  Card(7, "S"),
  Card(8, "S"),
  Card(9, "S"),
   Card(10, "S"),
   Card(11, "S"),
   Card(12, "S"),
   Card(13, "S"),
   Card(14, "S"),
  Card(1, "C"),
  Card(2, "C"),
  Card(3, "C"),
  Card(4, "C"),
  Card(5, "C"),
  Card(6, "C"),
  Card(7, "C"),
  Card(8, "C"),
  Card(9, "C"),
   Card(10, "C"),
   Card(11, "C"),
   Card(12, "C"),
   Card(13, "C"),
   Card(14, "C")
  )

  def deal: (Hand, Hand) = {
    val r = new Random()
    val hand1: Hand = Hand(r.shuffle(deck).take(5))
    val r2 = new Random(r.nextInt)
    val hand2: Hand = Hand(r2.shuffle(deck).take(5))

    (hand1, hand2)
  }

  def bestHand(hand: Hand): (Int, WinningMove) = {
    hand match {
      case h if h.isRoyalFlush => (10, "Royal Flush")
      case h if h.isStraightFlush => (9, "Straight Flush")
      case h if h.isFourOfAKind => (8, "Four Of A Kind")
      case h if h.isFullHouse._1 => (7 + h.isFullHouse._2, "Full House")
      case h if h.isFlush._1 => (6 + h.isFlush._2, "Flush")
      case h if h.isStraight._1 => (5 + h.isStraight._2, "Straight")
      case h if h.isThreeOfAKind._1 => (4 + h.isThreeOfAKind._2, "Three Of A Kind")
      case h if h.isTwoPair._1 => (3 + h.isTwoPair._2, "Two Pair")
      case h if h.isOnePair._1 => (2 + h.isOnePair._2, "One Pair")
      case x => (1, s"Highest card is ${x.highestCard}")
    }
  }

  def play(hand1: Hand, hand2: Hand): (Hand, WinningMove) = {
    println("Playing Poker!")
//    val (hand1, hand2) = deal
    println("*************")
    println(s"Player1 has $hand1")
    println("*************")
    println(s"Player2 has $hand2")
    println("*************")
    val hand1Best = bestHand(hand1)
    val hand2Best = bestHand(hand2)

    if (hand1Best._1 > hand2Best._1) {
      println(s"The winner is Player 1 with ${hand1Best._2}")
      (hand1, hand1Best._2)
    }

    else {
      println(s"The winner is Player 2 with ${hand2Best._2}")
      (hand2, hand2Best._2)
    }

  }
}
//
//object Main extends App {
//  Game.play
//}




