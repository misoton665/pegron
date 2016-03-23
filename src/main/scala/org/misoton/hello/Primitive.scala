package org.misoton.hello

import org.misoton.hello.ArithmeticalTextParser.{AST, Environment}

object Primitive {

  trait Primitive[T] {
    val value: T
  }

  trait NumericPrimitive[T] extends Primitive[T]{
    def +(right: NumericPrimitive[T]): NumericPrimitive[T]
    def -(right: NumericPrimitive[T]): NumericPrimitive[T]
    def *(right: NumericPrimitive[T]): NumericPrimitive[T]
    def /(right: NumericPrimitive[T]): NumericPrimitive[T]
    def %(right: NumericPrimitive[T]): NumericPrimitive[T]
    def **(right: NumericPrimitive[T]): NumericPrimitive[T]
    def ==(right: NumericPrimitive[T]): BooleanPrimitive
    def !=(right: NumericPrimitive[T]): BooleanPrimitive
  }

  case class IntPrimitive(left: Int) extends NumericPrimitive[Int] {
    type P = NumericPrimitive[Int]

    override def +(that: P): P = IntPrimitive(this.value + that.value)

    override def /(that: P): P = IntPrimitive(this.value / that.value)

    override def **(that: P): P = IntPrimitive(Math.pow(this.value, that.value).toInt)

    override def %(that: P): P = IntPrimitive(this.value % that.value)

    override def -(that: P): P = IntPrimitive(this.value - that.value)

    override def *(that: P): P = IntPrimitive(this.value * that.value)

    override def ==(that: P): BooleanPrimitive = BooleanPrimitive(this.value == that.value)

    override def !=(that: P): BooleanPrimitive = BooleanPrimitive(this.value != that.value)

    override val value: Int = left
  }

  case class BooleanPrimitive(left: Boolean) extends Primitive[Boolean] {
    type P = BooleanPrimitive

    def ||(that: P): P = BooleanPrimitive(this.value || that.value)

    def &&(that: P): P = BooleanPrimitive(this.value && that.value)

    override val value: Boolean = left
  }

  case class PrimitiveNode[T](pValue: Primitive[T]) extends AST {
    override def eval(env: Environment): AST = this
    def value(): Primitive[T] = pValue
  }
}
