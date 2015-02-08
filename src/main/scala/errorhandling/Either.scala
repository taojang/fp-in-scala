package errorhandling

import scala.{Either => _, Left => _, Right => _, _}

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f apply a)
    case Left(e)  => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f apply a
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    if (as.isEmpty)
      Right(Nil)
    else for {
      a <- f apply as.head
      t <- traverse(as)(f)
    } yield a :: t
}
