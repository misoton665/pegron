package org.misoton

object Hello {
  def main(args: Array[String]): Unit = {
    // Negative Number Calculation
    println(ArithmeticalTextParser("-3/-3"))
    println(ArithmeticalTextParser("-5-(-9--8)+-9"))

    // If Expression
    println(ArithmeticalTextParser("if( true ){ 1 + 2 }else{ 0 + 4 }"))
    println(ArithmeticalTextParser("if( false ){ 1 + 2 }else{ 0 + 4 }"))

    println(ArithmeticalTextParser("3 + 6 != 3 * 3"))

    println(ArithmeticalTextParser("var x = 9;"))

    val code =
      """var x = 1;
        |@x + if( 1 == 1 ){
        |   if(1 + 3 == 9){
        |     10
        |   }else{
        |     100
        |   }
        |}else{
        |   0
        |}
      """.stripMargin

    println(ArithmeticalTextParser(code))
  }
}
