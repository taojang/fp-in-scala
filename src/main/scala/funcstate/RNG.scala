trait RNG {

  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE6DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, r) => if (n == Int.MinValue) nonNegativeInt(r) else (Math.abs(n), r)
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, r) => if (n == Int.MaxValue) double(r) else (n / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG) = intDouble(rng) match {
    case ((i, d), r) => ((d, i), r)
  }

  def double3(rng: RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???
}
