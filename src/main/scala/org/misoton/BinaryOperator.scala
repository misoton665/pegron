package org.misoton

import org.misoton.ArithmeticalTextParser.{Environment, AST}
import org.misoton.Primitive.{Primitive, PrimitiveNode}

object BinaryOperator {
  type P[T] = Primitive[T]

  def operation[T](left: AST, right: AST, operate: (P[T], P[T]) => P[T], env: Environment): P[T] = {
    val lVal = left.eval(env) match {
      case PrimitiveNode(value) => value.asInstanceOf[P[T]]
    }

    val rVal = right.eval(env) match {
      case PrimitiveNode(value) => value.asInstanceOf[P[T]]
    }

    operate(lVal, rVal)
  }

  abstract class BinaryOperation[T](left: AST, right: AST) extends AST {
    def operate(l: P[T], r: P[T]): P[T]
    override def eval(env: Environment): AST = PrimitiveNode(operation[T](left, right, operate, env))
  }

  case class AddOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l + r
  }

  case class SubOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l - r
  }

  case class MulOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l * r
  }

  case class DivOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l / r
  }

  case class ModOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l % r
  }

  case class PowOp[T](left: AST, right: AST) extends BinaryOperation[T](left, right) {
    override def operate(l: P[T], r: P[T]): P[T] = l / r
  }

}
