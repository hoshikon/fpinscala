package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, tail) => tail
      case Nil => ???
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n-1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
      case Nil => ???
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
      case Nil => z
    }
  }

  def sumWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_+_)
  def productWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 1)(_*_)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b,a) => append(Cons(a, Nil), b))

  def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b,a)))(z)

  def foldRightWithFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, as) => Cons(a, as))

  def flatten[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil: List[A])((l, acc) => append(l, acc))

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, Nil: List[A])((acc, a) => {
      if (f(a)) Cons(a, acc) else acc
    })
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A]
    = flatMap(as)({a => { if (f(a)) List(a) else Nil: List[A] }})

  def zipInt(as: List[Int], bs: List[Int]): List[Int]
    = (as, bs) match {
      case (Cons(ha, ta), Cons(hb, tb)) => append(List(ha + hb), zipInt(ta, tb))
      case (Cons(_, _), Nil) => as
      case (Nil, Cons(_, _)) => bs
      case (Nil, Nil) => Nil
    }

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    (as, bs) match {
      case (Cons(ha, ta), Cons(hb, tb)) => append(List(f(ha,hb)), zipWith(ta, tb)(f))
      case (Cons(_, _), Nil) => as
      case (Nil, Cons(_, _)) => bs
      case (Nil, Nil) => Nil
    }
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(ha, ta), Cons(hb, tb)) => (ha == hb) && startsWith(ta, tb)
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(ha, ta), Cons(hb, tb)) => {
        if (ha == hb) startsWith(ta, tb)
        else hasSubsequence(ta, sub)
      }
    }
  }
}
