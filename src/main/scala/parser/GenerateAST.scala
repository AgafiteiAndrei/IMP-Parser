package parser

import entity.AST
import fastparse._
import entity.Statemant._


object GenerateAST {

  def apply(input: List[String])  = {
    var ast: List[AST.stmt] = List()

    input.foreach(e=>{
      if(input.count(_.trim.take(4).equals("vars")) > 1) throw new Exception("Multiple declarations!! Just one line with declarations!")
      if(input.tail.count(_.trim.take(4).equals("vars")) > 0) throw new Exception("Declarations must be on the first line on your code!!")

      if(e.trim.take(2).equals("if")) {
        val Parsed.Success(result,_) = parse(e, if_stmt(_))
        ast = ast :+ result
      }
      else if(e.trim.take(5).equals("while")) {
        val Parsed.Success(result,_) = parse(e, while_stmt(_))
        ast = ast :+ result
      }
      else if(e.trim.take(4).equals("vars")){
        val Parsed.Success(result,_) = parse(e, declaration(_))
        ast = ast :+ result
      }
      else if(e.trim.take(5).equals("print")){
        val Parsed.Success(result,_) = parse(e, printval(_))
        ast = ast :+ result
      }
      else
        {
          val Parsed.Success(result,_) = parse(e, assign(_))
          ast = ast :+ result
        }
    })

    ast

  }
}
