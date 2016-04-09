package chapters

import chapters.PropertyTesting._
import Prop._

import scala.languageFeature.postfixOps
import scala.util.matching.Regex


/**
  * Created on 2016-04-06.
  */
trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  // Exercise 9.4
  // Using map2 and succeed, implement the listOfN combinator
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List.empty[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // For a given parser, return a parser that returns a list of repeating elements that the parser matches
  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  // Exercise 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def defaultSucceed[A](a: A): Parser[A] = string("").map(_ => a)

  def succeed[A](a: A): Parser[A]

  // Returns the portion of the input the given parser p examined if it succeeded
  def slice[A](p: Parser[A]): Parser[String]

  // Exercise 9.1
  // same as many except must match at least once
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  // returns a parser that is the result of running p and p2 (if p was successful) and returning both their results as
  // a tuple (A, B)
  // Exercise 9.7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap {
      a => p2.map(b => (a, b))
    }
  // Exercise 9.1
  def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map {
      // note: could have used f.tupled which converts f into a g that takes all of f's args as a tuple
      case (a,b) => f(a, b)
    }

  // Exercise 9.7
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap {
      a => p2.map(f(a, _))
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  // similar to label but does not throw away labels attached to p
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  // Exercise 9.6
  val numParser = "[0-9]".r.flatMap(n => listOfN(n.toInt, char('a')))

  // Exercise 9.9
  def skipL[B](p: Parser[Any], p2: Parser[B]): Parser[B] =
    slice(p).map2(p2)((_,b) => b)

  def skipR[A](p: Parser[A], p2: Parser[Any]): Parser[A] =
    p.map2(slice(p2))((a, _) => a)


  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many:Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)
    def slice: Parser[String] = self.slice(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

}

object Impl {
  type Parser[+A] = Location => Result[A]
  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }

    def extract: Either[ParseError, A] = this match {
      case Success(a, m) => Right(a)
      case Failure(e, _) => Left(e)
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  object Parser extends Parsers[Parser] {

    // Exercise 9.13
    implicit def string(s: String): Parser[String] = (location: Location) =>
      if (location.input.substring(location.offset).startsWith(s))
        Success(s, s.length)
      else
        Failure(location.toError(s"Expected: $s"), true)

    implicit def regex(r: Regex): Parser[String] = (location: Location) =>
      r.findPrefixOf(location.input.substring(location.offset))
        .map(s => Success(s, s.length))
        .getOrElse(Failure(location.toError(s"Expected pattern: ${r}"), false))

    // similar to label but does not throw away labels attached to p
    def scope[A](msg: String)(p: Parser[A]): Parser[A] =
      s => p(s).mapError(_.push(s, msg))


    def label[A](msg: String)(p: Parser[A]): Parser[A] =
      s => p(s).mapError(_.label(msg))


    def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
      s => x(s) match {
          // If first branch was run in uncommitted state, run next branch
        case Failure(e, false) => y(s)
        case r => r
      }

    def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      s => p(s) match {
        case Success(a, n) => f(a)(s.advanceBy(n))
                                  .addCommit(n != 0)
                                  .advanceSuccess(n)
        case f: Failure => f
      }

    def attempt[A](p: Parser[A]): Parser[A] =
      s => p(s).uncommit

    // Returns the portion of the input the given parser p examined if it succeeded
    def slice[A](p: Parser[A]): Parser[String] = (l: Location) =>
      p(l) match {
        case Success(_, charsConsumed) => Success(l.input.substring(0, charsConsumed), charsConsumed)
        case f: Failure => f
      }

    def succeed[A](a: A): Parser[A] = (location: Location) => Success(a, 0)

    def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input, 0)).extract
  }
}

object JSONParser {
  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }
  
}