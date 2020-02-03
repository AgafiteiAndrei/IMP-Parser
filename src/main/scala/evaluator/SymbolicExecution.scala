package evaluator

import entity.AST
import evaluator.Evaluator.arithmetic_eval

object SymbolicExecution {

  def declaration_eval(decl: AST.stmt) = {
    var map: Map[String, ((String, Int),(String, Boolean))] = Map()
    decl match {
      case d: AST.stmt.Declaration =>{
        val lst = d.targets
        lst.foreach(e=>{
          map = map + (e.name -> ((e.name.toUpperCase().concat("!1"),0),("true",true)))
        })
      }
    }
    map
  }

  def se_expr(expr: AST.expr)(implicit map: Map[String, ((String, Int),(String,Boolean))], name:String):(String, Int) = {
    var local_map:Map[String, ((String, Int),(String,Boolean))] = map
    var result = ("",0)
    expr match {

      case num: AST.expr.Num => result = (num.n.toString, num.n.asInstanceOf[Int])

      case str: AST.expr.Str => {
        val a = local_map(str.s)._1
        val b = local_map(str.s)._2
        result = (a._1,a._2)
      }

      case add: AST.expr.ADD =>{
        val a = se_expr(add.left)
        val b = se_expr(add.right)
        val a_conc_b = a._1.concat(" + ").concat(b._1)
        val a_plus_b = a._2 + b._2
        result = (a_conc_b, a_plus_b)
      }
      case mul: AST.expr.MUL =>{
        val a = se_expr(mul.left)
        val b = se_expr(mul.right)
        val a_conc_b = a._1.concat(" * ").concat(b._1)
        val a_mul_b = a._2 * b._2
        result = (a_conc_b, a_mul_b)
      }
      case sub: AST.expr.SUB =>{
        val a = se_expr(sub.left)
        val b = se_expr(sub.right)
        val a_conc_b = a._1.concat(" - ").concat(b._1)
        val a_sub_b = a._2 - b._2
        result = (a_conc_b, a_sub_b)
      }
      case div: AST.expr.DIV =>{
        val a = se_expr(div.left)
        val b = se_expr(div.right)
        val a_conc_b = a._1.concat(" / ").concat(b._1)
        val a_div_b = a._2 / b._2
        result = (a_conc_b, a_div_b)
      }
      case mod: AST.expr.MOD =>{
        val a = se_expr(mod.left)
        val b = se_expr(mod.right)
        val a_conc_b = a._1.concat(" % ").concat(b._1)
        val a_mod_b = a._2 % b._2
        result = (a_conc_b, a_mod_b)
      }

    }
    result
  }

  def se_boolean(expr: AST.expr)(implicit map: Map[String, ((String, Int),(String,Boolean))], name:String) = {

  }

  def assign_eval(assign: AST.stmt)(implicit map: Map[String, ((String, Int),(String,Boolean))]) = {
    assign match {
      case a: AST.stmt.Assign =>{
        val name = a.target.name
        var local_map:Map[String, ((String,Int),(String,Boolean))] = map
        a.value match {
          case num: AST.expr.Num => {
            local_map = local_map.updated(name ,((name.toUpperCase.concat("!1"), num.n.asInstanceOf[Int]),(local_map.get(name).get._2._1,true)))
          }

          case _ =>{
            val l = se_expr(a.value)(map, name)
            local_map = local_map.updated(name, (l,local_map.get(name).get._2))
          }
        }

        local_map
      }
    }
  }


  def apply(ast: List[AST.stmt]) = {
    implicit var declarations: Map[String, ((String, Int),(String,Boolean))] = Map()
    ast.foreach{
      case d: AST.stmt.Declaration => declarations = declaration_eval(d)
      case a: AST.stmt.Assign => declarations = assign_eval(a)(declarations)
    }
    //declarations.foreach(println)
  }
}
