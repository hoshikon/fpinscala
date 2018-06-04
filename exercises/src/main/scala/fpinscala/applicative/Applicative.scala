package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2WithApply(fa, fb)(f)

  def map2WithApply[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)
//    apply(map[A, B => C](fa)(a => b => f(a, b)))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldLeft(unit(List.empty[A]))(map2(_, _)(_ :+ _))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(List.empty[A])
    else map2(fa, replicateM(n-1, fa))(_ :: _)

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)


  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fabgab: (F[A => B], G[A => B]))(faga: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fabgab._1)(faga._1), G.apply(fabgab._2)(faga._2))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def apply[A, B](fgab: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fgab, fga)(G.apply(_)(_))
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K,V]))((acc, kfv) =>
      map2(acc, kfv._2)(_.updated(kfv._1, _))
    )

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldLeft(unit(List.empty[A]))((mla, a) =>
      map2(mla, f(a))((la, b) => if (b) la :+ a else la)
    )
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  self =>
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

//  ***impossible to implement***
//  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
//    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
//    override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
//      self.map(ma)((ga: G[A]) => {
//        G.flatMap(ga)(a => self.pure(f(a)))
//      })
//    }
//  }
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = {
        (fab, fa) match {
          case (Success(ab), Success(a)) => Success(ab(a))
          case (Failure(h, t), Success(_)) => Failure(h, t)
          case (Success(_), Failure(h, t)) => Failure(h, t)
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h2, t2 ++ (h1 +: t1))
        }
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  import fpinscala.testing.{Prop, Gen}
  import Prop.forAll

  def applicativeLaws[FA[_], A, B](ap: Applicative[FA], genFaa: Gen[FA[A]], genAToB: Gen[A => B]): Prop = {

    val identityLaw = forAll(genFaa) { fa =>
      ap.map2[Unit, A, A](ap.unit(()), fa)((_,a) => a) == ap.unit(fa) &&
      ap.map2[A, Unit, A](fa, ap.unit(()))((a,_) => a) == ap.unit(fa)
    }

    def assoc[C,D,E](p: (C,(D,E))): ((C,D), E) = p match { case (a, (b, c)) => ((a,b), c) }
    val associativeLaw = forAll(Gen.listOfN(3, genFaa)) { case List(fa, fb, fc) =>
      ap.product(ap.product(fa, fb), fc) == ap.map(ap.product(fa, ap.product(fb,fc)))(assoc)
    }

    def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) = (i,i2) => (f(i), g(i2))
    val naturalityLaw = forAll(Gen.listOfN(2, genFaa).map2(Gen.listOfN(2, genAToB))((_, _))) { case (List(fa, fb), List(f, g)) =>
      ap.map2(fa, fb)(productF(f,g)) == ap.product(ap.map(fa)(f), ap.map(fb)(g))
    }

    identityLaw && associativeLaw && naturalityLaw
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence[G,B](map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

//  def map[A,B](fa: F[A])(f: A => B): F[B] =
//    traverse[({type f[x] = Either[Nothing, x]})#f, A, B](fa)(a => Right(f(a)))(Monad.eitherMonad).right.get

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldLeft(G.unit(List.empty[B])){ (acc, a) => G.map2(acc, f(a))(_:+_) }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(Some.apply)
        case _ => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
