package entity

import fastparse._
import NoWhitespace._
import Lexical._
object Expression {

  def name[_:P] = Lexical.identifier
  def number[_:P] = P(Lexical.integer).map(AST.expr.Num)
  def string[_:P] = Lexical.letter.!.rep(1).map(_.mkString).map(AST.expr.Str)

  def Arithmetic_chain[_:P](p: => P[AST.expr], op: => P[AST.operator]) = P(" ".? ~ p ~ " ".? ~ (op ~ " ".? ~  p~" ".?).rep).map{
    case (left, right) =>{
      right.foldLeft(left){
        case (lhs,(AST.operator.Add, rhs))=> AST.expr.ADD(lhs, rhs)
        case (lhs,(AST.operator.Sub, rhs))=> AST.expr.SUB(lhs, rhs)
        case (lhs,(AST.operator.Div, rhs))=> AST.expr.DIV(lhs, rhs)
        case (lhs,(AST.operator.Mul, rhs))=> AST.expr.MUL(lhs, rhs)
        case (lhs,(AST.operator.Mod, rhs))=> AST.expr.MOD(lhs, rhs)

      }
    }
  }

  def Comparison_chain[_:P](p: => P[AST.expr], op: => P[AST.cmpop]):P[AST.expr] = P(" ".? ~ p ~ " ".? ~ op ~ " ".? ~ p ~" ".?).map{
    case (left, op ,right)=>{
      AST.expr.Compare(left, op, right)
    }
  }

  def arithmetic[_:P] = P(Arithmetic_chain(termen, Add | Sub))
  def termen[_:P] = P(Arithmetic_chain(string | number, Div | Mult | Mod ))

  def comparison[_:P] = P( Comparison_chain(arithmetic | string | number,comp_op))

  def not_test[_:P]: P[AST.expr] = {
    P(" ".? ~ kw("not") ~ "("~ " ".? ~ (comparison | string | number) ~ " ".? ~ ")" ~ " ".?).map(AST.expr.NOT(_))
  }

  def and_test[_: P] = P( (not_test | comparison).rep(1, sep = kw("and")) ).map{
    case Seq(x) => AST.expr.Bool(x)
    case xs => {
      AST.expr.BoolOp(AST.boolop.And, xs)
    }
  }

  def or_test[_:P] = P((and_test).rep(1,kw("or"))).map{
    case Seq(x) => AST.expr.Bool(x)
    case xs => {
      AST.expr.BoolOp(AST.boolop.Or, xs)
    }
  }

  def test[_:P] = P(or_test | and_test | not_test | comparison | string | number)

  def op[T, _:P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def Lt[_:P] = op("<", AST.cmpop.Lt)
  def Gt[_:P] = op(">", AST.cmpop.Gt)
  def Eq[_:P] = op("=", AST.cmpop.Eq)
  def NotEq[_:P] = op("!=", AST.cmpop.NotEq)
  def comp_op[_:P] = P(Lt | Gt | Eq | NotEq)

  def Add[_:P] = op("+", AST.operator.Add)
  def Sub[_:P] = op("-", AST.operator.Sub)
  def Mult[_:P] = op("*", AST.operator.Mul)
  def Div[_:P] = op("/", AST.operator.Div)
  def Mod[_:P] = op("%", AST.operator.Mod)
  def arithmetic_op[_:P] = P(Add | Sub | Mult | Div | Mod)


}
