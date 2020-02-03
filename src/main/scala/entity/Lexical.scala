package entity

import fastparse._
import NoWhitespace._

object Lexical {

  def digit[_:P] = P(CharIn("0-9"))
  def lower_case[_:P] = P(CharIn("a-z"))
  def upper_case[_:P] = P(CharIn("A-Z"))
  def letter[_:P] = P(lower_case | upper_case)

  def reserved_words = Set("vars", "if", "endif;", "while", "endwhile;", "and", "or", "not", "skip", "do", "then")

  def identifier[_:P] = P((letter|"_") ~ (letter | digit| "_").rep).!.filter(!reserved_words.contains(_)).map(AST.Id)

  def negatable[T, _: P](p: => P[T])(implicit ev: Numeric[T]) = (("+" | "-").?.! ~ p).map{
    case ("-", x) => ev.negate(x)
    case ("+" ,x) => x
    case (_, x) => x
  }

  def int_numbers[_:P] = P(digit.rep).!.map(_.toInt)

  def integer[_:P] = negatable[Int, Any](P(int_numbers))
  def kw [_:P](s: String) = P(s ~ !(letter | digit | "_"))

}
