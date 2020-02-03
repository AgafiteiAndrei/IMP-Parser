package functions

import entity.AST

object PrintAST {
  def apply(ast: List[AST.stmt]) = {
    var astResult: String = ""
    ast.foreach(e=>{
      astResult = astResult.concat(e.toString).concat("\n")
    })
    astResult
  }
}
