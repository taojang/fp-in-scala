package errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f apply a)
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map (f apply _) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map { a => Some(a) } getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap { a => if (f apply a) Some(a) else None }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    if (a.isEmpty)
      None
    else for {
      h <- a.head
      t <- sequence(a.tail)
    } yield h :: t

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    if (a.isEmpty)
      None
    else for {
      h <- f apply a.head
      t <- traverse(a.tail)(f)
    } yield h :: t

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

}
