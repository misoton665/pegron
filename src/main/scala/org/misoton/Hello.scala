package org.misoton

object Hello {
  def main(args: Array[String]): Unit = {
    // Negative Number Calculation
    println(ArithmeticalTextParser("-3/-3"))
    println(ArithmeticalTextParser("-5-(-9--8)+-9"))

    // If Expression
    println(ArithmeticalTextParser("if( true ){ 1 + 2 }else{ 0 + 4 }"))
    println(ArithmeticalTextParser("if( false ){ 1 + 2 }else{ 0 + 4 }"))

    val code =
      """if( true ){
        |   2*-3/-3+-9
        |}else{
        |   0
        |}
      """.stripMargin

    println(ArithmeticalTextParser(code))
  }
}
