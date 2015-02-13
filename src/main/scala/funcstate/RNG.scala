package funcstate

object FuncState {

  case class State[S,+A](run: S => (A,S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(run andThen (a => (f apply a._1).run(a._2)))

    def map[B](f: A => B): State[S,B] = flatMap(f andThen State.unit)

    def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] = flatMap { a =>
      sb.map { b => f(a,b) }
    }
  }

  object State {

    def unit[S,B](b: B): State[S,B] = State((s: S) => (b, s))

    def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] =
      fs.foldRight(State.unit(Nil): State[S, List[A]])((a, b) => a.map2(b)(_ :: _))

  }

  trait RNG {

    def nextInt: (Int, RNG)
  }

  type Rand[+A] = State[RNG, A]

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE6DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // def unit[A](a: A): Rand[A] = rng => (a, rng)

  // def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //   rng => {
  //     val (a, rng2) = s(rng)
  //     (f(a), rng2)
  //   }

  // def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f andThen unit)

  // def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
  //   // val (a, r1) = ra(rng)
  //   // val (b, r2) = rb(r1)
  //   // (f(a, b), r2)
  //   val (fb, r) = map(ra)(a => f(a, _: B))(rng)
  //   map(rb)(b => fb(b))(r)
  // }

  // def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)({ a =>
  //   map(rb)(b => f(a,b))
  // })

  // def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
  //   val (a, r) = f(rng)
  //   g(a)(r)
  // }

  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //   fs.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))



  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = ra.map2(rb)((_, _))
  

  object SimpleRNG {

    val int: Rand[Int] = State(_.nextInt)

    val double: Rand[Double] = nonNegativeInt.map { i =>
      if (i == Int.MaxValue) (i - 1) / Int.MaxValue // TODO: fix the failure
      else i / Int.MaxValue
    }

    // def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    //   case (n, r) => if (n == Int.MaxValue) double(r) else (n / Int.MaxValue, r)
    // }

    val randIntDouble: Rand[(Int, Double)] = both(int, double)

    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    val nonNegativeInt: Rand[Int] = int.map { i =>
      if (i == Int.MinValue) i + 1 // TODO: fix the failure
      else Math.abs(i)
    }

    val nonNegativeEven: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

    val intDouble: Rand[(Int, Double)] = int.flatMap { i => double.map(d => (i,d)) }

    val doubleInt: Rand[(Double, Int)] = intDouble.map({ case (i, d) => (d, i) })

    val double3: Rand[(Double, Double, Double)] = double.flatMap { d1 =>
      double.flatMap { d2 => double.map( d3 => (d1, d2, d3)) }
    }

    def ints(count:Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

    // def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    //   sequence(List.fill(count)({ r: RNG => r.nextInt }))(rng)
    
    // def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    //   def intsRecurr(c: Int)(acc: List[Int])(rng: RNG): (List[Int], RNG) = {
    //     if (c < 1) (acc, rng)
    //     else {
    //       val (n, r) = rng.nextInt
    //       intsRecurr(c - 1)(n :: acc)(r)
    //     }
    //   }
    
    //   intsRecurr(count)(Nil)(rng)
    // }

    // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    //   fs match {
    //     case Nil => (Nil, rng)
    //     case h :: t => map2(h, sequence(t))(_ :: _)(rng)
    //   }
    // }

  }

}

