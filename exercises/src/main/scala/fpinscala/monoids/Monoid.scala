package fpinscala.monoids

import fpinscala.monoids.IndexedSeqFoldable.concatenate
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1
    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
//  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val law1 = forAll(Gen.listOfN(3, gen)){ case List(a1, a2, a3) => {
      m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
    }}
    val law2 = forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a))
    law1 && law2
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)


  // write foldLeft and foldRight using foldMap
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, new Monoid[B => B] {
    override def zero: B => B = identity
    override def op(a1: B => B, a2: B => B): B => B = a2 andThen a1
  })(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(a => f(_, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = as.length
    if (length == 0) m.zero
    else if (length == 1) f(as.head)
    else {
      val (as1, as2) = as.splitAt(length / 2)
      m.op(foldMapV(as1, m)(f), foldMapV(as2, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = foldMapV(ints, new Monoid[Option[(Int, Int)]] {
    override def op(a1: Option[(Int, Int)], a2: Option[(Int, Int)]): Option[(Int, Int)] = for {
      (min1, max1) <- a1
      (min2, max2) <- a2
      if max1 <= min2
    } yield (min1, max2)

    override def zero: Option[(Int, Int)] = Some((Int.MinValue, Int.MinValue))
  })(a => Some((a, a))).isDefined

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def zero: Par[A] = Par.unit(m.zero)
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(Par.fork(a1), Par.fork(a2))(m.op)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(Par.asyncF(f))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(chars1), Stub(chars2)) => Stub(chars1 + chars2)
      case (Stub(chars), Part(lst, words, rst)) => Part(chars + lst, words, rst)
      case (Part(lst, words, rst), Stub(chars)) => Part(lst, words, rst + chars)
      case (Part(lst1, words1, rst1), Part(lst2, words2, rst2)) => Part(lst1, words1 + zeroOrOne(rst1 + lst2) + words2, rst2)
    }
  }

  private def zeroOrOne(str: String): Int = if (str.isEmpty) 0 else 1

  def count(s: String): Int = {
    def charToWC(c: Char) = c.toString match {
      case " " => Part("", 0, "")
      case char => Stub(char)
    }

    foldMapV(s.toIndexedSeq, wcMonoid)(charToWC) match {
      case Part(lst, w, rst) => zeroOrOne(lst) + w + zeroOrOne(rst)
      case Stub(char) => zeroOrOne(char)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def zero: (A, B) = (A.zero, B.zero)
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def zero: A => B = (_: A) => B.zero
      override def op(a1: A => B, a2: A => B): A => B = (a: A) => B.op(a1(a), a2(a))
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(new Monoid[B => B] {
      val endo: Monoid[B => B] = endoMonoid[B]
      def op(x: B => B, y: B => B): B => B  = endo.op(y, x)
      val zero: B => B = endo.zero
    })(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldLeft(as)(List.empty[A])((acc, a) => a :: acc).reverse
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as match {
      case h :: t => f(h, foldRight(t)(z)(f))
      case _ => z
    }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as match {
      case h :: t => foldLeft(t)(f(z, h))(f)
      case _ => z
    }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    if (as.isEmpty) z else f(as.head, foldRight(as.tail)(z)(f))

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    if (as.isEmpty) z else foldLeft(as.tail)(f(z, as.head))(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    if (as.isEmpty) z else f(as.head, foldRight(as.tail)(z)(f))

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    if (as.isEmpty) z else foldLeft(as.tail)(f(z, as.head))(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    concatenate(as.map(f))(mb)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).getOrElse(mb.zero)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.map(f(z, _)).getOrElse(z)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.map(f(_, z)).getOrElse(z)
}

