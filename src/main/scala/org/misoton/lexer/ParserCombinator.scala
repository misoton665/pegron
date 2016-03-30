package org.misoton.lexer

object ParserCombinator {

  case class State(input: String, pos: Int)

  case class ParseError(message: String, state: State) extends RuntimeException

  type ParseResult[T] = Either[ParseError, (T, String)]

  abstract class Parser[+T] extends (String => ParseResult[T]) {

    var name: String = ""

    def named(name: String): Parser[T] = {
      this.name = name
      this
    }

    override def toString(): String = name

    def ~[R](that: Parser[R]): Parser[(T, R)] = parserGen((in) => {
      this(in) match {
        case Right((a, in2)) => that(in2) match {
          case Right((b, in3)) => Right(((a, b), in3))
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
    }).named(this.name + "~" + that.name)

    def ~>[R](that: Parser[R]): Parser[R] = parserGen((in) => {
      (this ~ that)(in) match {
        case Right(((_, a), in2)) => Right((a, in2))
        case Left(e) => Left(e)
      }
    }).named(this.name + "~>" + that.name)

    def <~[R](that: Parser[R]): Parser[T] = parserGen((in) => {
      (this ~ that)(in) match {
        case Right(((a, _), in2)) => Right((a, in2))
        case Left(e) => Left(e)
      }
    }).named(this.name + "<~" + that.name)
  }

  def parserGen[T](f: String => ParseResult[T]): Parser[T] =
    new Parser[T] {
      def apply(input: String): ParseResult[T] = f(input)
    }

  def parseAll[T](input: String, parser: Parser[T]): Either[ParseError, T] = {
    parser(input) match {
      case Right((tree, left)) => if(left.length == 0) Right(tree) else Left(ParseError("Left some string", State(left, 0)))
      case Left(e) => Left(e)
    }
  }

  implicit def string2parser(str: String): Parser[String] = parserGen((in) => {
    if(in startsWith str) Right((str, in substring str.length))
    else Left(ParseError("hoge", State("d", 0)))
  }).named(str)
}
