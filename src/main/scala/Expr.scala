sealed trait Expr
case class Assign(name: String, value: Expr) extends Expr
case class Str(string: String) extends Expr
case class Variable(name: String) extends Expr
case class Number(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Binary(lhs: Expr, op: String, rhs: Expr) extends Expr
case class Identifier(name: String) extends Expr

sealed trait Operator
case object Plus extends Operator
case object Div extends Operator
case object Minus extends Operator
