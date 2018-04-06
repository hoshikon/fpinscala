package fpinscala.laziness


import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() +: t().toList
    case Empty => List.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = if (n == 0) Empty else {
    this match {
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = if (n == 0) this else {
    this match {
      case Cons(_, t) => t().drop(n-1)
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, acc) => f(a).append(acc))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s).takeWhile(_._2.isDefined).forAll{case (a,b) => a == b }

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }

  def takeWithUnfold(n: Int): Stream[A] = if (n == 0) Empty else unfold(this){
    case Cons(h,t) => Some((h(), t().takeWithUnfold(n-1)))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) if p(h()) => Some(h(), t().takeWhileWithUnfold(p))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(),t2())))
      case (_, Cons(h2,t2)) => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1,t1), _) => Some(((Some(h1()), None), (t1(), empty)))
      case _ => None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipAll(s2).takeWhileWithUnfold(s => s._1.isDefined && s._2.isDefined).map(s => (s._1.get, s._2.get))

  def tails: Stream[Stream[A]] = unfold(this){
      case s@Cons(_,t) => Some((s, t()))
      case _ => None
    }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  //This is less efficient because our Stream has a lazy head, unlike scala default stream...
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((a, acc) => {
      lazy val acc2 = acc
      cons(f(a, acc2.headOption.get), acc2)
    })


  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val acc2 = acc
      val b = f(a, acc2._1)
      (b, cons(b, acc2._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val onesWithConstant: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(n: Int, m: Int): Stream[Int] = cons(n, loop(m, n+m))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map{ case (a,s) => cons(a, unfold(s)(f)) }
      .getOrElse(empty)

  def fibsWithUnfold: Stream[Int] = unfold((0,1)){ case (a, b) => Some(a, (b, a+b))}
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))
  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x,x))
  def onesWithUnfold: Stream[Int] = unfold(1)(a => Some(a,a))

}