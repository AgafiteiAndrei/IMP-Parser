package entity

object AST {
  case class Id(name: String)

  sealed trait boolop
  object boolop{
    case object And extends boolop
    case object Or extends boolop
  }

  sealed trait operator
  case object operator{
    case object Add extends operator
    case object Sub extends operator
    case object Mul extends operator
    case object Div extends operator
    case object Mod extends operator
  }

  sealed trait cmpop
  object cmpop{
    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object Gt extends cmpop
  }

  sealed trait expr
  object expr{
    case class BoolOp(operator: boolop, values: Seq[expr]) extends expr
    //case class IfExp(condition: expr, body: expr, orelse: expr) extends expr
    case class Compare(left: expr, ops: cmpop, right: expr) extends expr
    case class Num(n: Any) extends  expr
    case class Str(s : String) extends expr
    case class Bool(n: Any) extends expr
    //case class Assign(target: Id, value: expr) extends expr

    case class ADD(left: expr, right: expr) extends expr
    case class SUB(left: expr, right: expr) extends expr
    case class MUL(left: expr, right: expr) extends expr
    case class DIV(left: expr, right: expr) extends expr
    case class MOD(left: expr, right: expr) extends expr
    case class AND(left: Seq[expr], right: Seq[expr]) extends expr
    case class OR(left: Seq[expr], right: Seq[expr]) extends expr
    case class NOT(target: expr) extends expr

  }

  sealed  trait stmt
  object stmt{
    case class Assign(target: Id, value: expr) extends stmt
    case class While(condition: expr, body: Seq[stmt]) extends stmt
    case class If(condition: expr, body: Seq[stmt], orelse: Option[Seq[stmt]]) extends stmt
    case class Declaration(targets: Seq[Id]) extends stmt
    case class Print(target: expr) extends stmt
  }

  sealed trait unaryop
  object unaryop{
    case object Not extends unaryop
  }


}
