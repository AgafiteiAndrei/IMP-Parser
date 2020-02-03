package utils

import evaluator.Evaluator
import functions.PrintAST
import input_file.InputFileParser
import parser.GenerateAST

object GenerateResult {

  def get_resultEvaluator(path: String): String ={
    val input = InputFileParser(path)
    val ast = GenerateAST(input)
    Evaluator(ast)
  }

  def get_AST(path: String): String = {
    val input = InputFileParser(path)
    val ast = GenerateAST(input)
    PrintAST(ast)
  }

}
