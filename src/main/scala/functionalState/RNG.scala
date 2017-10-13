package functionalState

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, seed) = rng.nextInt
    if (num < 0) (Math.abs(num + 1), seed) else (num, seed)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, seed) = (nonNegativeInt(rng)._1.toDouble / Int.MaxValue, nonNegativeInt(rng)._2)
    if (num >= 1.0) (num - 0.1, seed) else (num, seed)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    val (d, finalRng) = double(nextRng)
    ((i, d), finalRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def loop(count: Int, loopRng: RNG): (List[Int], RNG) = {
      if (count == 0) (List(loopRng.nextInt._1), loopRng.nextInt._2)
      else {
        val (tail, finalRng) = loop(count-1, loopRng.nextInt._2)
        (loopRng.nextInt._1 :: tail, finalRng)
      }
    }
    loop(count,rng)
  }

  type Rand[+A] = RNG => (A,RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a:A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def mapDouble: Rand[Double] = {
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    // (rand[int], rand[double])((int, double) => (int,double)) => rand[(int,double)]
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}


