package fpinscala.parsing

import scala.util.matching.Regex

object MyParser {
  type Parser[+A] = Location => Result[A]

  object MyParsers extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
      case Success(a, n) => Right(a)
      case Failure(err, _) => Left(err)
    }

    override def succeed[A](a: A): Parser[A] = (_: Location) => Success(a, 0)


    override implicit def string(s: String): Parser[String] = scope("Unmatched string") { (l: Location) =>
      if (l.input.drop(l.offset).startsWith(s)) Success(s, s.length)
      else Failure(l.toError(s"expected: '$s' but was: '${l.input.slice(l.offset, l.offset + s.length)}'"), false)
    }

    override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = (l: Location) =>
      s1(l) match {
        case Failure(e, false) => s2(l)
        case r => r
      }

    override def slice[A](p: Parser[A]): Parser[String] = (l: Location) =>
      p(l) match {
        case Success(_, consumed) => Success(l.input.take(consumed), consumed)
        case f@Failure(_, _) => f
      }

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = (l: Location) =>
      p(l) match {
        case Success(a, n) => f(a)(l.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case f@Failure(_,_) => f
      }

    override implicit def regex(r: Regex): Parser[String] = (l: Location) => {
      val str = l.input.drop(l.offset)

      r.findPrefixOf(str) match {
        case Some(s) => Success(s, s.length)
        case _ => Failure(l.toError(s"expected: '$r' but started from '${str.take(3)}...'"), false)
      }
    }

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = (l: Location) => p(l).mapError(_.label(msg))

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = (l: Location) => p(l).mapError(_.push(l,msg))

    override def attempt[A](p: Parser[A]): Parser[A] = (l: Location) => p(l).uncommit
  }
}
