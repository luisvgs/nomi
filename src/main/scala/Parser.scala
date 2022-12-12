import fastparse._
import NoWhitespace._

class Parser {
  def number[_: P]: P[Expr] = P(
    CharIn("0-9").rep(1).!.map(x => Number {
      x.toInt
    })
  )

  def bool[_: P] : P[Expr] = {
    P( ("true" | "false").rep(1).!.map(x => Bool {
      x.toBoolean
    }))
  }

  def expr[_: P]: P[Expr] = {
    P(factor | expr_)
  }

  def letter[_: P]: P[_] = CharIn("A-Z")
  def identifier[_: P]: P[Expr] = {
    P("$" | "_" | "\\" | letter | number ).rep.!.map(Identifier)
  }

  // def foo :: a, b, c => ... end
  def fn_decl[_: P]= P("def" ~ identifier ~ "::" ~ (identifier ~ ",").rep.? ~ "=>" ~ statement ~ "end")

  def factor[_: P]: P[Expr]= {
    P(expr_ ~ CharIn("+\\-").! ~ number).map(Binary)
  }

  def statement[_:P]: P[_] = P((expr).rep)
  def expr_[_: P]: P[Expr] = {
    P(number | bool)
  }

}
