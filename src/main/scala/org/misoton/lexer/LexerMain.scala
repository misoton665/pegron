package org.misoton.lexer

object LexerMain {
  def main(args: Array[String]): Unit = {
    val a = 1 to 10
    val b = 1 to 10

    val c = for(i <- a; j <- b) yield i + ":" + j
    val d: () => Int = ???
    c.foreach(println)
  }
}
