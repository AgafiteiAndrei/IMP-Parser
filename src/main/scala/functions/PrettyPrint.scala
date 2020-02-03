package functions

import entity.AST
import parser.GenerateAST

object PrettyPrint {

  var pp_expr, pp_stmt = ""

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

  def eval_stmt(input: AST.stmt): String = {
    input match {
      case id: AST.Id => pp_stmt = id.name
      case decl: AST.stmt.Declaration =>{
        val lst = decl.targets
        var res = "vars "
        for(i<-0 until lst.size){
          if(i+1 == lst.size) res = res.concat(lst(i).name.concat(";"))
          else res = res.concat(lst(i).name.concat(", "))
        }
        pp_stmt = res
      }

      case assign: AST.stmt.Assign => {
        val target = assign.target
        val value = eval_expr(assign.value)
        var res = ""
        res = res.concat(target.name).concat(" := ").concat(value).concat(";")
        pp_stmt = res
      }

      case iff: AST.stmt.If =>{
        val test = eval_expr(iff.condition)
        var b1, b2 = ""
        var res = "if("
        iff.body.foreach(e=>{
          b1 = b1.concat(eval_stmt(e)).concat("\n")
        })

        if(iff.orelse.isDefined){
          iff.orelse.get.foreach(e=>{
            b2 = b2.concat(eval_stmt(e)).concat("\n")
          })
        }

        if(b2.nonEmpty) res = res.concat(test).concat(") then \n").concat(b1).concat("else\n").concat(b2).concat("endif;")
        else res = res.concat(test).concat(") then \n").concat(b1).concat("endif;")

        pp_stmt = res
      }

      case whilee: AST.stmt.While =>{
        val test = eval_expr(whilee.condition)
        var b1= ""
        var res = "while("
        whilee.body.foreach( e=>{
          b1 = "\t".concat(b1.concat(eval_stmt(e)).concat("\n"))
        })

        res = res.concat(test).concat(") do \n").concat(b1).concat("endwhile;")
        pp_stmt = res
      }

      case prnt: AST.stmt.Print =>{
        val value = eval_expr(prnt.target)
        var res = "print("
        res = res.concat(value).concat(");")
        pp_stmt = res
      }

    }

    pp_stmt
  }

//  def apply(ast: List[AST.stmt]) = {
//    ast.foreach{
//      case st: AST.stmt => println(eval_stmt(st))
//      case ex: AST.expr => println(eval_expr(ex))
//    }
//
//  }

}
