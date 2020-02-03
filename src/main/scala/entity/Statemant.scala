package entity

import fastparse._
import Lexical._
import Expression._
import NoWhitespace._

object Statemant {

  def word_comma[_:P] = P(" ".? ~ identifier ~ ",")
  def word_semicolon[_:P] = P(" ".? ~ identifier ~ " ".? ~ ";")

  def Chain_declaration[_:P](p1: => P[AST.Id], p2: => P[AST.Id]):P[AST.stmt] = P(p1.rep ~ p2).map{
    case (x,y) => {
      AST.stmt.Declaration(x :+ y)
    }
  }

  def declaration[_:P] = P("vars" ~ Chain_declaration(word_comma, word_semicolon))

  def assign[_:P]:P[AST.stmt] = P(identifier ~ " ".? ~ ":=" ~ " ".? ~ (arithmetic | string | number) ~ ";").map{
    case (a,b) => {
      AST.stmt.Assign(a,b)
    }
  }

  def block[_:P] = P(while_stmt | if_stmt | printval | assign)

  def if_stmt[_:P]:P[AST.stmt] = P(kw("if") ~ "(" ~ " ".? ~/test ~ " ".? ~ ") then "
                                                    ~ (" ".? ~ block ~ " ".?).rep
                                                    ~ (" ".? ~ "else" ~ (" ".? ~  block ~ " ".?).rep).?
                                                    ~ " ".? ~ "endif;"
  ).map{
    case (test, b1, b2) => {
      AST.stmt.If(test, b1, b2)
    }
  }

  def while_stmt[_:P]:P[AST.stmt] = P(kw("while") ~ "(" ~ " ".? ~/test ~ " ".? ~ ") do " ~ (" ".? ~ block ~ " ".?).rep ~ " ".? ~ "endwhile;").map{
    case (test, body) => {
      AST.stmt.While(test, body)
    }
  }

  def printval[_:P] = P(kw("print") ~ "(" ~ " ".? ~ arithmetic ~ " ".? ~ ");").map(AST.stmt.Print)

}
