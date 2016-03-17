package org.misoton

import org.misoton.ArithmeticalTextParser.{Environment, AST}
import org.misoton.Primitive.{PrimitiveNode, Primitive}

object Variable {
  case class DefinePrimitiveVariable(name: String, primitive: AST) extends AST{
    override def eval(env: Environment): AST = {
      env.variable.put(name, primitive)
      primitive
    }
  }

  case class LookPrimitiveVariable(name: String) extends AST{
    override def eval(env: Environment): AST = {
      env.variable.get(name)
    }
  }
}
