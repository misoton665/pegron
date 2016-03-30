import org.scalatest._

import org.misoton.lexer.ParserCombinator._

class HelloSpec extends FlatSpec with Matchers {
  "String Literal" should "be parser" in {
    val parser = "hello"
    parser.toString should be("hello")
  }

  "String Literal" should "parse itself" in {
    val parser: Parser[String] = "hello"
    parser("hello") should be(Right("hello", ""))
  }

  "Connected Parser with \"~\"" should "be Parser" in {
    val parser1 = "hello"
    val parser2 = "parser"
    (parser1 ~ parser2).toString should be("hello~parser")
  }

  "Connected Parser with \"~>\"" should "be Parser" in {
    val parser1 = "hello"
    val parser2 = "parser"
    (parser1 ~> parser2).toString should be("hello~>parser")
  }

  "Connected Parser with \"<~\"" should "be Parser" in {
    val parser1 = "hello"
    val parser2 = "parser"
    (parser1 <~ parser2).toString should be("hello<~parser")
  }

  "Connected Parser with \"~\"" should "parse connected string" in {
    val parser1 = "hello"
    val parser2 = "parser"
    parseAll("helloparser", parser1 ~ parser2) should be(Right(("hello", "parser")))
  }

  "Connected Parser with \"~>\"" should "parse connected string" in {
    val parser1 = "hello"
    val parser2 = "parser"
    parseAll("helloparser", parser1 ~> parser2) should be(Right("parser"))
  }

  "Connected Parser with \"<~\"" should "parse connected string" in {
    val parser1 = "hello"
    val parser2 = "parser"
    parseAll("helloparser", parser1 <~ parser2) should be(Right("hello"))
  }

  "Over string parsing" should "be error" in {
    val parser1 = "hello"
    val parser2 = "parser"
    parseAll("helloparser_", parser1 ~ parser2) should be(Left(ParseError("Left some string", State("_", 0))))
  }
}
