import Main._

class Interpreter(ast: List[Expr]) {
  def interpret(): Value = eval(ast)

  private def eval(exprs: Seq[Expr]): Value = {
    var res: Value = Nothing()
    exprs.foreach { expr =>
      res = stmt_eval(expr)
    }
    res
  }

  private def get_op(op: String): Operator = op match {
    case "+" => Plus
    case "/" => Div
    case "-" => Minus
  }

  private def stmt_eval(expr: Expr): Value = expr match {
    case Number(x) => Integer(x)
    case Bool(b) => Booli(b)
    case Binary(lhs: Expr, op: String, rhs: Expr) => {
      val x: Value = stmt_eval(lhs)
      val y: Value = stmt_eval(rhs)
      get_op(op) match {
       case Plus => x + y
       case Minus => x - y
      }
    }
    case _ => ???
  }
}
