import Main._
import scala.collection.mutable.ArrayBuffer

class Interpreter(var env: Environment) {
  def eval(exprs: Seq[Expr]): Value = {
    var res: Value = Nothing()
    exprs.foreach { expr =>
      res = stmt_eval(expr).getOrElse(Nothing())
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

  private def eval_block(
      stmts: Seq[Expr],
      env: Environment
  ): Either[String, Value] = {
    var value: Value = Nothing()
    var prev = this.env

    val steps = () => {
      this.env = env

      stmts.foreach { case stmt =>
        value = stmt_eval(stmt).getOrElse(Nothing())
      }

      value
    }

    val result = steps()
    this.env = prev

    Right(result)
  }

  private def stmt_eval(expr: Expr): Either[String, Value] = expr match {
    case Number(x) => Right(Integer(x))
    case Str(s)    => Right(StrLiteral(s))
    case Func(name, arg, stmt) => {
      val fn = Fn(arg, stmt)

      println(s"<fn>: ", name)
      this.env.define(name, fn)
      Right(Nothing())
    }
    case Call(fn, stmt) => {
      var values = ArrayBuffer[Value]()

      stmt.foreach(x => {
        values += stmt_eval(x).getOrElse(Nothing())
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
      val res: Value = stmt_eval(v).right.get
      this.env.define(n, res)
      Right(Nothing())
    }
    case Identifier(name) =>
      this.env.resolve(name) match {
        case Some(value) => Right(value)
        case None        => Left("Variable was not found in scope")

      }
    case Bool(b) => Right(Booli(b))
    case Binary(lhs: Expr, op: String, rhs: Expr) => {
      val x: Value = stmt_eval(lhs).getOrElse(Nothing())
      val y: Value = stmt_eval(rhs).getOrElse(Nothing())
      get_op(op) match {
        case Plus        => Right(x + y)
        case Minus       => Right(x - y)
        case LessThan    => Right(x < y)
        case GreaterThan => Right(x > y)
        case And         => Right(x && y)
        case Or          => Right(x || y)
        case Equal       => Right(x == y)
      }
    }
    case _ => ???
  }
}
