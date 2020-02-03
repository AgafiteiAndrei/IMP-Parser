package utils
import evaluator.Evaluator
import functions.{PrettyPrint_v2, PrintAST}
import input_file.InputFileParser
import javafx.application.Application
import parser.GenerateAST



object Main {
  def main(args: Array[String]): Unit = {
    Application launch classOf[App]

//    val input = InputFileParser("test.txt")
//    val ast = GenerateAST(input)
//    PrettyPrint_v2(ast)
//      println(PrintAST(ast))
  }
}






