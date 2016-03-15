package org.misoton

import scala.util.parsing.combinator.RegexParsers

object ArithmeticalTextParser extends RegexParsers{

  class Environment {
  }

  trait AST {
    def eval(env: Environment): AST
  }

  case class AddOp(left: AST, right: AST) extends AST {
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(lVal + rVal)
    }
  }
  case class SubOp(left: AST, right: AST) extends AST{
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(lVal - rVal)
    }
  }
  case class MulOp(left: AST, right: AST) extends AST{
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(lVal * rVal)
    }
  }
  case class DivOp(left: AST, right: AST) extends AST{
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(lVal / rVal)
    }
  }
  case class ModOp(left: AST, right: AST) extends AST{
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(lVal % rVal)
    }
  }
  case class PowOp(left: AST, right: AST) extends AST{
    override def eval(env: Environment): AST = {
      val lVal = left.eval(env) match {
        case IntVal(value) => value
      }

      val rVal = right.eval(env) match {
        case IntVal(value) => value
      }

      IntVal(Math.pow(lVal, rVal).toInt)
    }
  }
  case class IntVal(value: Int) extends AST {
    override def eval(environment: Environment): AST = {
      this
    }
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

  def number: Parser[AST] = """-?[1-9][0-9]*|0""".r ^^ {x => IntVal(x.toInt)}

  def RS = rep(space)
  def space = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  // Expression[END]

  def apply(input: String) = parseAll(expression, input) match {
    case Success(postalCodeData, next) => Right(postalCodeData.eval(new Environment))
    case NoSuccess(message, next) => Left("ERROR: " + message + " column " + next.pos.column + " in line " + next.pos.line)
  }
}
