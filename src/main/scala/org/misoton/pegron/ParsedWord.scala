package org.misoton.pegron

object ParsedWord {
  case class ~[+L, +R](left: L, right: R) {
    override def toString: String = left + "~" + right
  }

  implicit class ParsedWord[L](left: L) {
    def apply[R](right: R): ~[L, R] = {
      new ~(left, right)
    }
  }
}
