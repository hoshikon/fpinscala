package fpinscala.parsing

import fpinscala.testing._

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  implicit def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String] = string(s1) or string(s2)
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: => Parser[A]): Parser[List[A]] = if (n > 0) map2(p, listOfN(n-1, p))(_ :: _) else succeed(List.empty)

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  val numA: Parser[Int] = char('a').many.slice.map(_.length)
  val numB: Parser[Int] = char('b').many1.slice.map(_.length)
  val numAAndB: Parser[(Int, Int)] = map2(numA, numB)((_,_))
  def slice[A](p: Parser[A]): Parser[String] //return the portion of the input string examined by the parser if successful
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = flatMap(p)(a => p2.map((a, _)))
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2).map { case (a: A, b: B) => f(a, b) }
  def map3[A,B,C,D](p: Parser[A], p2: => Parser[B], p3: Parser[C])(f: (A, B, C) => D): Parser[D] = (p ** p2 ** p3).map { case ((a: A, b: B), c: C) => f(a, b, c) }
  def map2WithFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = flatMap(p)(a => p2.map(f(a, _)))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def regex(r: Regex): Parser[String]
  def singleDigitFollowedByAs: Parser[String] = "^[0-9]a*".r.flatMap(str => succeed(str.take(1)) ** listOfN(str.head.toInt, char('a')).slice).map { case (h,t) => h + t }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def errorLocation(e: ParseError): Location = e.stack.head._1
  def errorMessage(e: ParseError): String = e.stack.head._2
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = map2(p, many)(_ :: _)|succeed(List.empty)
    def many1: Parser[List[A]] = map2(p, many)(_ :: _)
    def map[B](f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))  //map2WithFlatMap(p, string(""))((a,_) => f(a))
    def slice: Parser[String] = self.slice(p)
    def **[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def isAssociative[A, B>:A, C>:B](str: String)(a: Parser[A], b: Parser[B], c: Parser[C]): Boolean =
      run(a|(b|c))(str) == run((a|b)|c)(str)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
    def productLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = equal(p1 ** p2, (p2 ** p1).map(_.swap))(in)
    def productLaw2[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in){
      s =>
        (run(p1)(s), run(p1 ** p1)(s)) match {
          case (Right(a), Right((b, c))) => a == b && a == c
          case (a, bc) => a == bc
        }
    }
    def productLaw3[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(in: Gen[String]): Prop = equal(p1 ** (p2 ** p3), (p1 ** p2) ** p3)(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
      run(label(msg)(p))(input) match {
        case Left(e) => errorMessage(e) == msg
        case _ => true
      }
    }

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def concat(p: ParseError): ParseError = ParseError(this.stack ::: p.stack, this.otherFailures ::: p.otherFailures)
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_,s)).toList)
  def latestLoc: Option[Location] = latest map (_._1)
  def latest: Option[(Location,String)] = stack.lastOption

  override def toString: String = {
    val mainMessage = stack.sortBy(_._1.offset).groupBy(_._1.offset).map{ case (_, list) =>
      val location = list.head._1
      s"Error found at line ${location.line}: '${location.currentLine}'\n${list.map("  " + _._2).mkString("\n")}"
    }.mkString("\n")
    val additionalMessage = if (otherFailures.nonEmpty) {
      "Other failures:\n" + otherFailures.map(_.toString)
    } else ""

    mainMessage + additionalMessage
  }
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, _) => Failure(e, 0)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, if (isCommitted) c + 1 else c)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, commits: Int) extends Result[Nothing]

//object StringParsers extends Parsers[StringParser] {
//  override def run[A](p: StringParser[A])(input: String): Either[String, A] = p.parse(input)
//  override def char(c: Char): StringParser[Char] = StringParser.char(c)
//  override implicit def string(s: String): StringParser[String] = StringParser.string(s)
//  override def orString(s1: String, s2: String): StringParser[String] = string(s1) | string(s2)
//  override def or[A](s1: StringParser[A], s2: => StringParser[A]): StringParser[A] = s1.or(s2)
//  override def listOfN[A](n: Int, p: => StringParser[A]): StringParser[List[A]] = StringParser.listOfN(n, p)
//
//  override def slice[A](p: StringParser[A]): StringParser[String] = ???
//
//  override def flatMap[A, B](p: StringParser[A])(f: A => StringParser[B]): StringParser[B] = ???
//
//  override implicit def regex(r: Regex): StringParser[String] = ???
//}
//
//case class StringParser[+A](parse: String => Either[String, A]) {
//  def or[B>:A](p: StringParser[B]): StringParser[B] = StringParser { str =>
//    parse(str) match {
//      case a@Right(_) => a
//      case Left(al) => p.parse(str) match {
//        case b@Right(_) => b
//        case Left(bl) => Left(s"$al, $bl")
//      }
//    }
//  }
//
//  def map[B](f: A => B): StringParser[B] = StringParser(str => parse(str).map(f))
//  def map2[B, C](b: StringParser[B])(f: (A, B) => C): StringParser[C] = flatMap(a => b.map(f(a, _)))
//  def flatMap[B](f: A => StringParser[B]): StringParser[B] = StringParser(str => parse(str).flatMap(f(_).parse(str)))
//}
//
//object StringParser {
//  def char(c: Char): StringParser[Char] = StringParser(input => if (c.toString == input) Right(c) else Left(s"Expected char '$c'"))
//  def string(s: String): StringParser[String] = StringParser(input => if (s == input) Right(s) else Left(s"Expected string '$s'"))
//
//  def sequence[A](l: List[StringParser[A]]): StringParser[List[A]] = StringParser(input => {
//    l.foldLeft((input, Right(List.empty[A]): Either[String, List[A]])) { case ((substring, either), p) =>
//      if (either.isLeft) ("", either)
//      else substring.inits.toSeq.reverse.find(p.parse(_).isRight)
//        .map(str => (substring.replaceFirst(str, ""), either.flatMap(l => p.parse(str).map(l :+ _))))
//        .getOrElse(("", Left[String, List[A]]("Unable to find parsable string")))
//    }._2
//  })
//
//  def listOfN[A](n: Int, p: StringParser[A]): StringParser[List[A]] = sequence(List.fill(n)(p))
//
//  def numOfChar(c: Char): StringParser[Int] = StringParser(input => Right(input.count(_==c)))
//
//  def numOfCharOneOrMore(c: Char): StringParser[Int] = StringParser(input => {
//    val n = input.count(_ == c)
//    if (n > 0) Right(n) else Left(s"Expected one or more '$c'")
//  })
//
//  def numOfTwoChars(a: Char, b: Char): StringParser[(Int, Int)] = numOfChar(a).map2(numOfCharOneOrMore(b))((_, _))
//}