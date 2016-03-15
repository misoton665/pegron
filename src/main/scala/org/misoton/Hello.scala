package org.misoton

object Hello {
  def main(args: Array[String]): Unit = {
    println(ArithmeticalTextParser("1 + 2 - 3 * 4 / 5 % 6 ** ( 7 + 8 )"))
    println(ArithmeticalTextParser("-5-(-9--8)+-9"))
  }
}
