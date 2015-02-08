package strictnesslaziness

import scala.{Stream => _, _}

trait Stream[+A] {

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = {
    if (n < 1) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n < 1) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n -1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
 
  def empty[A] = Empty

  def cons[A](h: => A, t: => Stream[A]) = {
    lazy val hd = h
    lazy val tl = t
    Cons(() => hd, () => tl)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))
}
