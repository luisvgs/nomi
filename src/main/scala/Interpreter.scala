import Main._
import scala.collection.mutable.ArrayBuffer

class Interpreter(var env: Environment) {
  def eval(exprs: Seq[Expr]): Value = {
    var res: Value = Nothing()
    exprs.foreach { expr =>
      res = stmt_eval(expr)
    }
    res
  }

  private def get_op(op: String): Operator = op match {
    case "+"  => Plus
    case "-"  => Minus
    case "==" => Equal
    case "||" => Or
    case "<"  => LessThan
    case ">"  => GreaterThan
    case "&&" => And
  }

  private def eval_block(stmts: Seq[Expr], env: Environment): Value = {
    var value: Value = Nothing()
    var prev = this.env

    val steps = () => {
      this.env = env

      stmts.foreach { case stmt =>
        value = stmt_eval(stmt)
      }

      value
    }

    val result = steps()
    this.env = prev

    result
  }

  private def stmt_eval(expr: Expr): Value = expr match {
    case Number(x) => Integer(x)
    case Str(s)    => StrLiteral(s)
    case Func(name, arg, stmt) => {
      val fn = Fn(arg, stmt)

      println(s"<fn>: ", name)
      this.env.define(name, fn)
      Nothing()
    }
    case Call(fn, stmt) => {
      var values = ArrayBuffer[Value]()

      stmt.foreach(x => {
        values += stmt_eval(x)
        values.toArray
      })

      val fn_defined = this.env.resolve(fn) match {
        case Some(v) => v
        case None => {
          println("nop")
          Nothing()
        }
      }

      fn_defined match {
        case Fn(args, stmt) => {
          var environment = this.env.from_ref(this.env)

          args.zip(values).foreach { case (param, argument) =>
            environment.define(param.toString, argument)
          }

          eval_block(stmt, environment)
        }
        case _ => ???
      }
    }
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
        case Plus        => x + y
        case Minus       => x - y
        case LessThan    => x < y
        case GreaterThan => x > y
        case And         => x && y
        case Or          => x || y
        case Equal       => x == y
      }
    }
    case _ => ???
  }
}
