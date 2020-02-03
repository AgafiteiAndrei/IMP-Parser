package evaluator

import entity.AST

object Evaluator {

  private var while_result: String = ""

  def declaration_eval(decl: AST.stmt) = {
    var map: Map[String, Int] = Map()
    decl match {
      case d: AST.stmt.Declaration => {
        val lst = d.targets
        lst.foreach(e => {
          map = map + (e.name -> 0)
        })
      }
    }
    map
  }

  def arithmetic_eval(arith: AST.expr)(implicit map: Map[String, Int]): Int = {
    var result = 0
    arith match {
      case nr: AST.expr.Num => result = nr.n.asInstanceOf[Int]
      case str: AST.expr.Str if str.s.equals("true") => result = 1
      case str: AST.expr.Str if str.s.equals("false") => result = 0
      case str: AST.expr.Str => {
        result = if (map.contains(str.s)) map(str.s) else throw new Exception("Variable " + str.s + " isn't declared.")
      }
      case add: AST.expr.ADD => result = arithmetic_eval(add.left) + arithmetic_eval(add.right)
      case sub: AST.expr.SUB => result = arithmetic_eval(sub.left) - arithmetic_eval(sub.right)
      case mul: AST.expr.MUL => result = arithmetic_eval(mul.left) * arithmetic_eval(mul.right)
      case div: AST.expr.DIV => result = arithmetic_eval(div.left) / arithmetic_eval(div.right)
      case mod: AST.expr.MOD => result = arithmetic_eval(mod.left) % arithmetic_eval(mod.right)
    }
    result
  }

  def assign_eval(assign: AST.stmt)(implicit map: Map[String, Int]) = {
    assign match {
      case a: AST.stmt.Assign => {
        val name = a.target.name
        val value = arithmetic_eval(a.value)(map)
        if (map.contains(name)) map.updated(name, value)
        else throw new Exception("Variable " + name + " isn't declare.")
      }
    }
  }

  def boolean_eval(bool: AST.expr)(implicit map: Map[String, Int]): Boolean = {
    bool match {
      case cmp: AST.expr.Compare => {
        val left = cmp.left
        val right = cmp.right
        val op = cmp.ops
        op.toString match {
          case "Lt" => arithmetic_eval(left) < arithmetic_eval(right)
          case "Gt" => arithmetic_eval(left) > arithmetic_eval(right)
          case "Eq" => arithmetic_eval(left) == arithmetic_eval(right)
          case "NotEq" => arithmetic_eval(left) != arithmetic_eval(right)

        }
      }
      case str: AST.expr.Str if str.s.equals("true") => true
      case str: AST.expr.Str if str.s.equals("false") => false
      case b: AST.expr.Bool => boolean_eval(b.n.asInstanceOf[AST.expr])

      case not: AST.expr.NOT => {
        val target = boolean_eval(not.target)
        !target
      }

      case and: AST.expr.BoolOp if and.operator.toString.equals("And") => {
        val lst = and.values.reverse
        var result = boolean_eval(lst(1))
        for (i <- 0 until lst.tail.size) {
          result = result & boolean_eval(lst(i))
        }
        result
      }

      case or: AST.expr.BoolOp if or.operator.toString.equals("Or") => {
        val lst = or.values.reverse
        var result = boolean_eval(lst(1))
        for (i <- 0 until lst.tail.size) {
          result = result | boolean_eval(lst(i))
        }
        result
      }
    }
  }

  def stmt_eval(st: AST.stmt)(implicit map: Map[String, Int]): (Map[String, Int], String) = {
    st match {
      case iff: AST.stmt.If => {
        var local_map: Map[String, Int] = map
        var finalResult = ""
        val test = boolean_eval(iff.condition)(map)
        val b1 = iff.body
        val b2 = iff.orelse

        if (test) {
          b1.foreach {
            case i: AST.stmt.If => {
              local_map = stmt_eval(i)(local_map)._1
              finalResult = finalResult.concat(stmt_eval(i)(local_map)._2)
            }
            case w: AST.stmt.While => {
              local_map = stmt_eval(w)(local_map)._1
              finalResult = finalResult.concat(stmt_eval(w)(local_map)._2)
            }
            case a: AST.stmt.Assign => local_map = assign_eval(a)(local_map)
            case p: AST.stmt.Print => {
              local_map = stmt_eval(p)(local_map)._1
              finalResult = finalResult.concat(stmt_eval(p)(local_map)._2)
            }
          }
        }
        else {
          if (b2.isDefined) {
            b2.get.foreach {
              case i: AST.stmt.If => {
                local_map = stmt_eval(i)(local_map)._1
                finalResult = finalResult.concat(stmt_eval(i)(local_map)._2)
              }

              case w: AST.stmt.While => {
                local_map = stmt_eval(w)(local_map)._1
                finalResult = finalResult.concat(stmt_eval(w)(local_map)._2)
              }
              case a: AST.stmt.Assign => local_map = assign_eval(a)(local_map)

              case p: AST.stmt.Print => {
                local_map = stmt_eval(p)(local_map)._1
                finalResult = finalResult.concat(stmt_eval(p)(local_map)._2)
              }
            }
          }
          else local_map = map
        }
        (local_map, finalResult)
      }

      case whilee: AST.stmt.While => {
        val test = whilee.condition
        var local_map: Map[String, Int] = map
        val body = whilee.body
        while (boolean_eval(test)(local_map)) {

          body.foreach {

            case i: AST.stmt.If => {
              local_map = stmt_eval(i)(local_map)._1
              while_result = while_result.concat(stmt_eval(i)(local_map)._2)
            }

            case w: AST.stmt.While => {
              local_map = stmt_eval(w)(local_map)._1
              while_result = while_result.concat(stmt_eval(w)(local_map)._2)
            }

            case a: AST.stmt.Assign => local_map = assign_eval(a)(local_map)

            case p: AST.stmt.Print => {
              local_map = stmt_eval(p)(local_map)._1
              while_result = while_result.concat(stmt_eval(p)(local_map)._2)
            }
          }
        }
        (local_map, while_result)
      }

      case prnt: AST.stmt.Print => {
        val value = arithmetic_eval(prnt.target)
        var local_map: Map[String, Int] = map
        var finalResult = ""
        prnt.target match {
          case str: AST.expr.Str => {
            finalResult = prnt.target.asInstanceOf[AST.expr.Str].s + " = " + value.toString.concat("\n")
          }
          case _ => {
            finalResult = value.toString
          }
        }
        (local_map, finalResult)
      }
    }
  }


  def apply(ast: List[AST.stmt]) = {
    var declarations: Map[String, Int] = Map()
    var evaluatorResult: String = ""
    ast.foreach {
      case d: AST.stmt.Declaration => declarations = declaration_eval(d)
      case a: AST.stmt.Assign => declarations = assign_eval(a)(declarations)
      case i: AST.stmt.If => {
        declarations = stmt_eval(i)(declarations)._1
        val result = stmt_eval(i)(declarations)._2
        if (result.size != 0) {
          evaluatorResult = evaluatorResult.concat(result)
        }
      }
      case w: AST.stmt.While => {
        declarations = stmt_eval(w)(declarations)._1
        val result = stmt_eval(w)(declarations)._2
        if (result.size != 0) {
          evaluatorResult = evaluatorResult.concat(result)
        }
      }
      case p: AST.stmt.Print => {
        declarations = stmt_eval(p)(declarations)._1
        val result = stmt_eval(p)(declarations)._2
        if (result.size != 0) {
          evaluatorResult = evaluatorResult.concat(result)
        }
      }
    }
    evaluatorResult
  }

}
