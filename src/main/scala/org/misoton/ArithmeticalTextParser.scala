package org.misoton

import org.misoton.BinaryOperator._
import org.misoton.Primitive.{IntPrimitive, PrimitiveNode}

import scala.util.parsing.combinator.RegexParsers

object ArithmeticalTextParser extends RegexParsers{

  class Environment {
  }

  trait AST {
    def eval(env: Environment): AST
  }

  // Expression[BEGIN]

  def expression: Parser[AST] = RS ~> additive <~ RS

  def additive = chainl1(multiplication,
    "+" ^^ {op => (left: AST, right: AST) => AddOp(left, right)}|
    "-" ^^ {op => (left: AST, right: AST) => SubOp(left, right)}
  )

  def multiplication: Parser[AST] = chainl1(power,
    "*" ^^ {op => (left: AST, right: AST) => MulOp(left, right)}|
    "/" ^^ {op => (left: AST, right: AST) => DivOp(left, right)}|
    "%" ^^ {op => (left: AST, right: AST) => ModOp(left, right)}
  )

  def power: Parser[AST] = chainl1(primary,
    "**" ^^ {op => (left: AST, right: AST) => PowOp(left, right)}
  )

  lazy val primary = "(" ~> RS ~> expression <~ RS <~ ")" ^^ {x => x} | number

  def number: Parser[AST] = """-?[1-9][0-9]*|0""".r ^^ {x => PrimitiveNode(IntPrimitive(x.toInt))}

  def RS = rep(space)
  def space = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  // Expression[END]

  def apply(input: String) = parseAll(expression, input) match {
    case Success(postalCodeData, next) => Right(postalCodeData.eval(new Environment))
    case NoSuccess(message, next) => Left("ERROR: " + message + " column " + next.pos.column + " in line " + next.pos.line)
  }
}
