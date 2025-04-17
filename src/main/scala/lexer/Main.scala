package lexer

import java.io.{File, FileOutputStream}
import scala.annotation.experimental
import scala.sys.exit

@main
def main(outputFilePath: String): Unit = {
  val lexer = Lexer("ab" -> (), "aba" -> ())
  lexer.lex("aba")

}
