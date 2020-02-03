package functions

import entity.AST
import functions.PrettyPrint.{eval_expr, eval_stmt, pp_expr}

object PrettyPrint_v2 {

  var index : Integer = 1

  def setUpIndex(i: Integer): Unit ={
    index = index + 1
  }
  def setDownIndex(): Unit ={
    index = index - 1
  }

  def getIndex(): Integer ={
    index
  }

  def eval_expr(input: AST.expr): String = {
    input match {
      case bool: AST.expr.Bool => pp_expr = eval_expr(bool.n.asInstanceOf[AST.expr])
      case str: AST.expr.Str => pp_expr = str.s
      case num: AST.expr.Num => pp_expr = num.n.toString
      case add: AST.expr.ADD => pp_expr = eval_expr(add.left).concat(" + ").concat(eval_expr(add.right))
      case sub: AST.expr.SUB => pp_expr = eval_expr(sub.left).concat(" - ").concat(eval_expr(sub.right))
      case mul: AST.expr.MUL => pp_expr = eval_expr(mul.left).concat(" * ").concat(eval_expr(mul.right))
      case div: AST.expr.DIV => pp_expr = eval_expr(div.left).concat(" / ").concat(eval_expr(div.right))
      case mod: AST.expr.MOD => pp_expr = eval_expr(mod.left).concat(" % ").concat(eval_expr(mod.right))
      case not: AST.expr.NOT => pp_expr = "not(".concat(eval_expr(not.target)).concat(")")
      case cmp: AST.expr.Compare if cmp.ops.toString.equals("Lt") => pp_expr = eval_expr(cmp.left).concat(" < ").concat(eval_expr(cmp.right))
      case cmp: AST.expr.Compare if cmp.ops.toString.equals("Gt") => pp_expr = eval_expr(cmp.left).concat(" > ").concat(eval_expr(cmp.right))
      case cmp: AST.expr.Compare if cmp.ops.toString.equals("Eq") => pp_expr = eval_expr(cmp.left).concat(" = ").concat(eval_expr(cmp.right))
      case cmp: AST.expr.Compare if cmp.ops.toString.equals("NotEq") => pp_expr = eval_expr(cmp.left).concat(" != ").concat(eval_expr(cmp.right))
      case and: AST.expr.BoolOp if and.operator.toString.equals("And") => {
        val lst = and.values
        var res = ""
        for (i <- 0 until lst.size) {
          if (i + 1 == lst.size) res = res.concat(eval_expr(lst(i)))
          else res = res.concat(eval_expr(lst(i))).concat(" and ")
        }
        pp_expr = res
      }

      case or: AST.expr.BoolOp if or.operator.toString.equals("Or") => {
        val lst = or.values
        var res = ""
        for (i <- 0 until lst.size) {
          if (i + 1 == lst.size) res = res.concat(eval_expr(lst(i)))
          else res = res.concat(eval_expr(lst(i))).concat(" or ")
        }

        pp_expr = res
      }
    }
    pp_expr
  }

  def eval_stmt(input: AST.stmt): String ={
    input match {
      case assign: AST.stmt.Assign => {
        val name = assign.target.toString
        var pp: String = "Assignments:" + "\n"
        val value = assign.value.toString
        pp = pp.concat("\t".concat(name + " = " + value))
        //println(pp)
        pp
      }

      case decl: AST.stmt.Declaration => {
        val targets = decl.targets
        var pp: String = "Declarations:" + "\n"
        for (i <- 0 until targets.size) {
          pp = pp.concat("\t".concat(targets(i).toString).concat("\n"))
        }
        //println(pp)
        pp
      }

      case iff: AST.stmt.If =>{
        val test = iff.condition
        val b1 = iff.body
        val b2 = iff.orelse
        var pp = "if(" + test.toString + ") then \n"

        b1.foreach(e=>{
          for(i<-0 until getIndex()){
            pp = pp.concat("\t")
          }
          pp = pp.concat(eval_stmt(e))
        })
        setDownIndex()
        if(b2.isDefined){
          for(i<-0 until getIndex()){
            pp = pp.concat("\t")
          }
          pp = pp.concat("else\n")
          setUpIndex(1)
          b2.get.foreach(e=>{
            for(i<-0 until getIndex()){
              pp = pp.concat("\t")
            }
            pp = pp.concat(eval_stmt(e))
          })
          setDownIndex()
          pp = pp.concat("endif;")
        }
        else {
          setDownIndex()
          pp = pp.concat("endif;")
        }

        println(pp)
        setUpIndex(1)
        pp
      }


      case prnt: AST.stmt.Print =>{
        val target = prnt.target
        val pp: String = "print(" + target.toString+")\n"
        //println(pp)
        pp
      }


    }
  }




  def apply(ast: List[AST.stmt]) = {
    ast.foreach{
      case st: AST.stmt => eval_stmt(st)
    }

  }

}
