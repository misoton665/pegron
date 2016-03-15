package org.misoton

import org.misoton.ArithmeticalTextParser.{Environment, AST}

object Primitive {

  trait Primitive[T] {
    def +(right: Primitive[T]): Primitive[T]
    def -(right: Primitive[T]): Primitive[T]
    def *(right: Primitive[T]): Primitive[T]
    def /(right: Primitive[T]): Primitive[T]
    def %(right: Primitive[T]): Primitive[T]
    def **(right: Primitive[T]): Primitive[T]
    val value: T
  }

  case class IntPrimitive(val left: Int) extends Primitive[Int] {
    override def +(that: Primitive[Int]): Primitive[Int] = IntPrimitive(this.value + that.value)

    override def /(that: Primitive[Int]): Primitive[Int] = IntPrimitive(this.value / that.value)

    override def **(that: Primitive[Int]): Primitive[Int] = IntPrimitive(Math.pow(this.value, that.value).toInt)

    override def %(that: Primitive[Int]): Primitive[Int] = IntPrimitive(this.value % that.value)

    override def -(that: Primitive[Int]): Primitive[Int] = IntPrimitive(this.value - that.value)

    override def *(that: Primitive[Int]): Primitive[Int] = IntPrimitive(this.value * that.value)

    override val value: Int = left
  }

  case class PrimitiveNode[T](pValue: Primitive[T]) extends AST {
    override def eval(env: Environment): AST = this
    def value(): Primitive[T] = pValue
  }
}
