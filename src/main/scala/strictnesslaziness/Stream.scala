package strictnesslaziness

import scala.{Stream => _, _}

trait Stream[+A] {

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // using unfold ex 5.13
  def map[B](f: A => B): Stream[B] = Stream.unfold(this)({
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  })

  def map2[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  def append(as: => Stream[A]): Stream[A] = foldRight(as)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // using unfold ex 5.13
  def take(n: Int): Stream[A] = Stream.unfold((n, this))({
    case (c, Cons(h, t)) => if (c > 1) Some(h(), (n - 1, t())) else None
    case _ => None
  })

  def take2(n: Int): Stream[A] = {
    if (n < 1) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take2(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n < 1) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n -1)
    }
  }

  // using unfold ex 5.13
  def takeWhile(p: A => Boolean): Stream[A] = Stream.unfold(this)({
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
    case _ => None
  })

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C) = Stream.unfold((this, s))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  })

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).foldRight(true)({ (el, acc) =>
    el match {
      case (Some(e1), Some(e2)) => if (e1 == e2) true && acc else false
      case (None, Some(e2)) => false
      case (Some(e1), None) => true
    }
  })
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

  def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))

  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

  def fibs: Stream[Int] = {
    def fibsRecurr(x: Int, y: Int): Stream[Int] = Cons(() => x + y, () => fibsRecurr(y, x + y))
    apply(0, 1, 2).append(fibsRecurr(1, 2))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map({ case (a, s) => Cons(() => a, () => unfold(s)(f)) }).getOrElse(Empty)
}
