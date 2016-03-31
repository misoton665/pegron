package org.misoton.lexer

import org.misoton.lexer.ParserCombinator._

object LexerMain {
  def main(args: Array[String]): Unit = {
    println("Run LexerMain")

    val space = " " / "\n" / "\t"

    val spacing = space.*

    val number = """([1-9]?[0-9]+)""".r

    val additive_op = "+" / "-"

    val additive = number ~ (spacing ~> additive_op <~ spacing) ~ number

    val expression = additive

    println(parseAll("1 + 2", expression))
  }
}
