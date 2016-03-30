import org.scalatest._

import org.misoton.lexer.ParserCombinator._

class HelloSpec extends FlatSpec with Matchers {
  "String Literal" should "be parser" in {
    val parser = "hello"
    parser.toString should be("hello")
  }

  "String Literal" should "parse itself" in {
    val parser: Parser[String] = "hello"
    parseAll("hello", parser) should be(Right("hello"))
  }

  "Unmatched string literal parsing" should " be error" in {
    val parser: Parser[String] = "hello"
    parseAll("hey", parser) should be(Left(ParseError("hey cannot match with hello", State("hey", 0))))
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

  "Connected Parser (a ~ b ~ c)" should "parse (abc) to (abc)" in {
    val parser1 = "hello"
    val parser2 = "parser"
    val parser3 = "hoge"
    parseAll("helloparserhoge", parser1 ~ parser2 ~ parser3) should be(Right((("hello", "parser"), "hoge")))
  }

  "Connected Parser (a ~> b <~ c)" should "parse (abc) to (b)" in {
    val parser1 = "hello"
    val parser2 = "parser"
    val parser3 = "hoge"
    parseAll("helloparserhoge", parser1 ~> parser2 <~ parser3) should be(Right("parser"))
  }

  "Over string parsing" should "be error" in {
    val parser = "hello"
    parseAll("hello_", parser) should be(Left(ParseError("Left some string", State("_", 5))))
  }
}
