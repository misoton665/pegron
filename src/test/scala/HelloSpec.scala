import org.misoton.pegron.ParsedWord.~
import org.scalatest._
import org.misoton.pegron.ParserCombinator._

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

  "Regex Literal" should "parse itself" in {
    val parser = """([a-zA-Z]+)""".r
    parseAll("hogehogefugapiyo", parser) should be(Right("hogehogefugapiyo"))
  }

  "Unmatched regex literal parsing" should " be error" in {
    val parser = """([a-zA-Z]+)""".r
    parseAll("hogehogefugapiyo0", parser) should be(Left(ParseError("Left some string", State("0", 16))))
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
    parseAll("helloparser", parser1 ~ parser2) should be(Right(new ~("hello", "parser")))
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
    parseAll("helloparserhoge", parser1 ~ parser2 ~ parser3) should be(Right(new ~(new ~("hello", "parser"), "hoge")))
  }

  "Connected Parser (a ~> b <~ c)" should "parse (abc) to (b)" in {
    val parser1 = "hello"
    val parser2 = "parser"
    val parser3 = "hoge"
    parseAll("helloparserhoge", parser1 ~> parser2 <~ parser3) should be(Right("parser"))
  }

  "Parser (!a)" should "parse (a) to be error" in {
    val parser = "hello"
    parseAll("hello", !parser) should be(Left(ParseError("Parsing succeed, but excepted to failure: hello", State("", 5))))
  }

  "Parser (!a ~ b)" should "parse (b) to be success" in {
    val parser1 = "hello"
    val parser2 = "hey"
    parseAll("hey", !parser1 ~ parser2) should be(Right(new ~((), "hey")))
  }

  "Parser (a / b)" should "parse (b) to be success" in {
    val parser1 = "hello"
    val parser2 = "hey"
    parseAll("hey", parser1 / parser2) should be(Right("hey"))
  }

  "Parser (a / b)" should "parse (a) to be success" in {
    val parser1 = "hello"
    val parser2 = "hey"
    parseAll("hello", parser1 / parser2) should be(Right("hello"))
  }

  "Parser (a*)" should "parse (aa) to be success" in {
    val parser1: Parser[String] = "hello"
    parseAll("hellohello", parser1.*) should be(Right(List("hello", "hello")))
  }

  "Parser (a*)" should "parse () to be success" in {
    val parser1: Parser[String] = "hello"
    parseAll("", parser1.*) should be(Right(List()))
  }

  "Parser (a+)" should "parse (aa) to be success" in {
    val parser1: Parser[String] = "hello"
    parseAll("hellohello", parser1.+) should be(Right(List("hello", "hello")))
  }

  "Parser (a+)" should "parse () to be not success" in {
    val parser1: Parser[String] = "hello"
    parseAll("", parser1.+) should be(Left(ParseError(" cannot match with hello", State("", 0))))
  }

  "Parser (a?)" should "parse () to be success" in {
    val parser1: Parser[String] = "hello"
    parseAll("", parser1.?) should be(Right(None))
  }

  "Parser (a?)" should "parse (a) to be success" in {
    val parser1: Parser[String] = "hello"
    parseAll("hello", parser1.?) should be(Right(Some("hello")))
  }

  "Over string parsing" should "be error" in {
    val parser = "hello"
    parseAll("hello_", parser) should be(Left(ParseError("Left some string", State("_", 5))))
  }

  "String \"1\" be evaluated to 1" should "be success" in {
    val parser = """([0-9]+)""".r
    parseAll("1", parser ^^ {(x) => x.toInt}) should be(Right(1))
  }
}
