import Main._

class Interpreter(env: Environment) {
  def eval(exprs: Seq[Expr]): Value = {
    var res: Value = Nothing()
    exprs.foreach { expr =>
      res = stmt_eval(expr)
    }
    res
  }

  private def get_op(op: String): Operator = op match {
    case "+" => Plus
    case "-" => Minus
    case "==" => Equal
    case "||" => Or
    case "<" => LessThan
    case ">" => GreaterThan
    case "&&" => And
  }

  private def stmt_eval(expr: Expr): Value = expr match {
    case Number(x) => Integer(x)
    case Str(s)    => StrLiteral(s)
    case Assign(n, v) => {
      val res = stmt_eval(v)
      this.env.define(n, res)
      Nothing()
    }
    case Identifier(name) =>
      this.env.resolve(name) match {
        case Some(value) => value
        case None        => Nothing()

      }
    case Bool(b) => Booli(b)
    case Binary(lhs: Expr, op: String, rhs: Expr) => {
      val x: Value = stmt_eval(lhs)
      val y: Value = stmt_eval(rhs)
      get_op(op) match {
        case Plus  => x + y
        case Minus => x - y
        case LessThan => x < y
        case GreaterThan => x > y
        case And => x && y
        case Or => x || y
        case Equal => x == y
      }
    }
    case _ => ???
  }
}
