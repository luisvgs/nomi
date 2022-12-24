import fastparse._
import NoWhitespace._

class Parser {
  def space[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 0))

  def number[_: P]: P[Expr] = P(CharIn("0-9").rep(1).!.map(x => Number {
    x.toInt
  })
  )

  def variable[_:P]: P[Expr] = P(identifier).!.map(Variable)
  def assignment[_:P]: P[Expr] = P("let" ~ space ~ identifier.! ~ space ~ "=" ~ expr).map(Assign.tupled)

  def bool[_: P]: P[Expr] = {
    P(("true" | "false").rep(1).!.map(x => Bool {
      x.toBoolean
    }))
  }

  def expr[_: P]: P[Expr] = {
    P(factor | expr_)
  }

  def letter[_: P]: P[_] = CharIn("A-Z")

  def identifier[_: P]: P[Expr] = {
    P("$" | "_" | "\\" | letter | number).rep.!.map(Identifier)
  }

  // def foo :: a, b, c => ... end
  def fn_decl[_: P] = P("def" ~ space ~ identifier ~ space ~ "::" ~ space ~ (identifier ~ ",").rep.? ~ space ~ "=>" ~ statement ~ "end")

  def factor[_: P]: P[Expr] = {
    P(expr_ ~ space ~ CharIn("+\\-").! ~ space ~ number).map(Binary.tupled)
  }

  def statement[_: P]: P[Seq[Expr]] = P((expr).rep)

  def expr_[_: P]: P[Expr] = {
    P(number | bool | assignment )
  }

}
