sealed trait Expr
case class Assign(name: String, value: Expr) extends Expr
case class Str(string: String) extends Expr
case class Func(name: String, args: String, stmt: Seq[Expr]) extends Expr
case class Variable(name: String) extends Expr
case class Number(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Binary(lhs: Expr, op: String, rhs: Expr) extends Expr
case class Identifier(name: String) extends Expr
case class Call(fn: String, stmt: Seq[Expr]) extends Expr

sealed trait Operator
case object Plus extends Operator
case object Minus extends Operator
case object Or extends Operator
case object And extends Operator
case object LessThan extends Operator
case object GreaterThan extends Operator
case object Equal extends Operator
