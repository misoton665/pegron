package org.misoton

object Hello {
  def main(args: Array[String]): Unit = {
    println(ArithmeticalTextParser("-3/-3"))
    println(ArithmeticalTextParser("-5-(-9--8)+-9"))
  }
}
