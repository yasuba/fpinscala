def sumLists(l: List[Int], l2: List[Int]): List[Int] = {
  l.zip(l2).map(x => x._1 + x._2)
}

sumLists(List(1,2,3),List(4,5,6))


def zipWith[A,B](l: List[A], l2: List[B])(f: (A,B) => A): List[A] =
  l.zip(l2).map(x => f(x._1, x._2))

zipWith(List(1,2,3),List(4,5,6))(_ + _)

def hasSubsequence[A](mainList: List[A], subList: List[A]): Boolean = {
  def loop(n:A):Boolean =
    mainList.contains(n)
  subList.forall( i => loop(i) )
}

hasSubsequence2(List(1,2,3,4), List(1,2))

hasSubsequence2(List(1,2,3,4), List(2,3))

hasSubsequence2(List(1,2,3,4), List(4))

hasSubsequence(List(1), List())

def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if sup.startsWith(sub) => true
  case h :: t => hasSubsequence2(t, sub)
}
