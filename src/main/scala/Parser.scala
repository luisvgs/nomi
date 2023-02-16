import fastparse._
import NoWhitespace._

class Parser {
  def stringChars(c: Char) = c != '\"' && c != '\\'
  def hexDigit[_: P] = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  def strChars[_: P] = P(CharsWhile(stringChars))

  def space[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 0))

  def string[_: P] =
    P("\"" ~/ strChars.rep.! ~ "\"").map(Str)

  def number[_: P]: P[Expr] = P(
    CharIn("0-9")
      .rep(1)
      .!
      .map(x =>
        Number {
          x.toInt
        }
      )
  )

  def assignment[_: P]: P[Expr] =
    P("let" ~ space ~ letter.rep(1).! ~ space ~ "=" ~ space ~ expr ~ End)
      .map(Assign.tupled)

  def bool[_: P]: P[Expr] = {
    P(
      ("true" | "false")
        .rep(1)
        .!
        .map(x =>
          Bool {
            x.toBoolean
          }
        )
    )
  }

  def letter[_: P]: P[_] = CharIn("aA-zZ")

  def keyword[_: P]: P[_] = StringIn("let", "def")
  def operator[_: P]: P[_] = CharIn("+\\-\\||\\&&\\<\\>\\==")

  def identifier[_: P]: P[Expr] = {
    P(!keyword ~ letter.rep(1).!).map(Identifier)
  }

  def fn_decl[_: P]: P[Expr] = P(
    StringIn("def") ~ space ~ letter
      .rep(1)
      .! ~ space ~ "::" ~ space ~ letter.rep.! ~ space ~ "=>" ~ space ~ statement ~ End
  ).map(Func.tupled)

  def reify(operand: Expr, rest: Seq[(String, Expr)]): Expr = {
    rest match {
      case Nil => operand
      case (op, other) :: xs =>
        Binary(operand, op, reify(other, xs))
    }
  }

  def factor[_: P]: P[Expr] = {
    P(primary ~ space ~ (CharIn("+").! ~ space ~ primary).rep)
      .map { case (op, rest) =>
        reify(op, rest)
      }
  }
  // def factor[_: P]: P[Expr] = {
  //   P(primary ~ (CharIn("+").! ~ space ~ primary).rep)
  //     .map {
  //       case (lhs, rhs) =>
  //         rhs match {
  //           case Nil               => lhs
  //           case (op, other) :: xs => Binary(lhs, op, other)
  //         }
  //       case (lhs, _) => lhs
  //     }
  // }

  def call[_: P]: P[Expr] = {
    P(letter.rep(1).! ~ "(" ~ statement ~ ")")
      .map(Call.tupled)
  }

  def statement[_: P]: P[Seq[Expr]] = P((expr).rep)

  def expr[_: P]: P[Expr] = {
    P(equality | fn_decl | call | assignment)
  }

  def equality[_: P]: P[Expr] = {
    P(factor)
  }

  def primary[_: P]: P[Expr] = {
    P(number | bool | identifier)
  }
}
