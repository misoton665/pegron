package org.misoton.hello

import org.misoton.hello.ArithmeticalTextParser.{AST, Environment}
import org.misoton.hello.Primitive.{BooleanPrimitive, Primitive, PrimitiveNode}

object Expression {

  case class IfExp(cond: AST, positive: AST, negative: AST) extends AST {
    override def eval(env: Environment): AST = {
      cond.eval(env) match {
        case PrimitiveNode(primitive) => primitive.asInstanceOf[Primitive[Boolean]] match {
          case BooleanPrimitive(true) => positive.eval(env)
          case BooleanPrimitive(false) => negative.eval(env)
        }
      }
    }
  }

  case class ForExp(init :AST, cond: AST, post: AST, statement: AST) extends AST {
    override def eval(env: Environment): AST = {
      init.eval(env)
      loop(cond, post, statement, env)
    }

    def loop(cond: AST, post: AST, statement: AST, env: Environment): AST = {
      cond.eval(env) match {
        case PrimitiveNode(primitive) => primitive.asInstanceOf[BooleanPrimitive] match {
          case BooleanPrimitive(true) =>
            statement.eval(env)
            loop(cond, post, statement, env)
          case BooleanPrimitive(false) => PrimitiveNode(BooleanPrimitive(false))
        }
      }
    }
  }
}
