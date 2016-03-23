package org.misoton.hello

object BinaryOperator {
  type P[T] = Primitive[T]
  type NP[T] = NumericPrimitive[T]

  abstract class NumericBinaryOperation[T](left: AST, right: AST) extends AST {
    def operate(l: NP[T], r: NP[T]): P[T]

    def operation(env: Environment): P[T] = {
      val lVal = left.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[NP[T]]
      }

      val rVal = right.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[NP[T]]
      }

      operate(lVal, rVal)
    }

    override def eval(env: Environment): AST = PrimitiveNode(operation(env))
  }

  abstract class BooleanBinaryOperation(left: AST, right: AST) extends AST {
    def operate(l: BooleanPrimitive, r: BooleanPrimitive): BooleanPrimitive

    def operation(env: Environment): BooleanPrimitive = {
      val lVal = left.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[BooleanPrimitive]
      }

      val rVal = right.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[BooleanPrimitive]
      }

      operate(lVal, rVal)
    }

    override def eval(env: Environment): AST = PrimitiveNode(operation(env))
  }

  abstract class LogicalBinaryOperation[T](left: AST, right: AST) extends AST {
    def operate(l: NP[T], r: NP[T]): BooleanPrimitive

    def operation(env: Environment): BooleanPrimitive = {
      val lVal = left.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[NP[T]]
      }

      val rVal = right.eval(env) match {
        case PrimitiveNode(value) => value.asInstanceOf[NP[T]]
      }

      operate(lVal, rVal)
    }

    override def eval(env: Environment): AST = PrimitiveNode(operation(env))
  }

  case class AddOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l + r
  }

  case class SubOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l - r
  }

  case class MulOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l * r
  }

  case class DivOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l / r
  }

  case class ModOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l % r
  }

  case class PowOp[T](left: AST, right: AST) extends NumericBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): NP[T] = l / r
  }

  case class EqOp[T](left: AST, right: AST) extends LogicalBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): BooleanPrimitive = l == r
  }

  case class UneqOp[T](left: AST, right: AST) extends LogicalBinaryOperation[T](left, right) {
    override def operate(l: NP[T], r: NP[T]): BooleanPrimitive = l != r
  }

  case class LogicalAndOp[T](left: AST, right: AST) extends BooleanBinaryOperation(left, right) {
    override def operate(l: BooleanPrimitive, r: BooleanPrimitive): BooleanPrimitive = l && r
  }

  case class LogicalOrOp[T](left: AST, right: AST) extends BooleanBinaryOperation(left, right) {
    override def operate(l: BooleanPrimitive, r: BooleanPrimitive): BooleanPrimitive = l || r
  }

}
