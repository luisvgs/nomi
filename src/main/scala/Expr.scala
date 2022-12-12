sealed trait Expr
case class Number(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Binary(lhs: Expr, op: String, rhs: Expr) extends Expr
case class Identifier(name: String) extends Expr
