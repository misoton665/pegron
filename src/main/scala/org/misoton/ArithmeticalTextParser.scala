package org.misoton

import org.misoton.BinaryOperator._
import org.misoton.Expression.IfExp
import org.misoton.Primitive.{BooleanPrimitive, IntPrimitive, PrimitiveNode}

import scala.util.parsing.combinator.RegexParsers

object ArithmeticalTextParser extends RegexParsers{

  class Environment {
  }

  trait AST {
    def eval(env: Environment): AST
  }

  // Expression[BEGIN]

  def expression: Parser[AST] = RS ~> (boolExpression | arithmeticExpression | controlExpression) <~ RS

  def arithmeticExpression: Parser[AST] = additive

  def controlExpression: Parser[AST] = ifExp

  def boolExpression: Parser[AST] = bool_op | bool

  def ifExp: Parser[AST] = "if(" ~ RS ~ boolExpression ~ RS ~ "){" ~ RS ~ expression ~ RS ~ "}else{" ~ RS ~ expression ~ RS ~ "}" ^^
    {case _~_~ cond ~_~_~_~ positive ~_~_~_~ negative ~_~_ => IfExp(cond, positive, negative)}


  def bool_op: Parser[AST] = chainl1(arithmeticExpression,
    "==" ^^ {op => (left: AST, right: AST) => EqOp(left, right)}|
    "!=" ^^ {op => (left: AST, right: AST) => UneqOp(left, right)})

  def bool: Parser[AST] = "true"  ^^ {lit => PrimitiveNode(BooleanPrimitive(true))}|
                          "false" ^^ {lit => PrimitiveNode(BooleanPrimitive(false))}

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

  lazy val primary = "(" ~> RS ~> expression <~ RS <~ ")" ^^ {x => x} | number | controlExpression

  def number: Parser[AST] = """-?[1-9][0-9]*|0""".r ^^ {x => PrimitiveNode(IntPrimitive(x.toInt))}

  def PS = space ~ RS
  def RS = rep(space)
  def space = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  // Expression[END]

  def apply(input: String) = parseAll(expression, input) match {
    case Success(postalCodeData, next) => Right(postalCodeData.eval(new Environment))
    case NoSuccess(message, next) => Left("ERROR: " + message + " column " + next.pos.column + " in line " + next.pos.line)
  }
}
