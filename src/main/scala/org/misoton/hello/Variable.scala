package org.misoton.hello

import org.misoton.hello.ArithmeticalTextParser.{AST, Environment}

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
