import Main._
import scala.collection.mutable.ArrayBuffer

class Interpreter(var env: Environment) {
  def eval(exprs: Seq[Expr]): Either[String, Value] = {
    var res: Either[String, Value] = Right(Nothing());
    exprs.map(expr => res = stmt_eval(expr))

    res match {
      case Right(v) => Right(v)
      case Left(e)  => Left(e)
    }
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
        value = stmt_eval(stmt).right.get
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

      this.env.define(name, fn)
      Right(Nothing())
    }
    case Call(fn, args) => {
      var values = ArrayBuffer[Value]()

      args.foreach(x => {
        values += stmt_eval(x).getOrElse(Nothing())
        values.toArray
      })

      val fn_defined = this.env.resolve(fn) match {
        case Some(v) => v
        case None => {
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
        case _ => Left("Que verga")
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
