import org.scalatest.funsuite.AnyFunSuite
import fastparse._

class NomiTest extends AnyFunSuite {
  val Parsed.Success(value, _) = parse("123", new Parser().number(_))
  test("1 should be number") {
    assert(value === Number(123))
  }

  val Parsed.Success(t, _) = parse("true", new Parser().bool(_))
  test("true should be boolean") {
    assert(t === Bool(true))
  }

  val Parsed.Success(l, _) = parse("3 < 10", new Parser().comparison(_))
  test("3 less than 10") {
    assert(l === Binary(Number(3), "<", Number(10)))
  }

  val Parsed.Success(f, _) = parse("false", new Parser().bool(_))
  test("false should be boolean") {
    assert(f === Bool(false))
  }

  val Parsed.Success(b, _) = parse("12 + 32", new Parser().factor(_))
  test("expression is binary") {
    assert(b === Binary(Number(12), "+", Number(32)))
  }

  val Parsed.Success(a, _) = parse("let foo = 32", new Parser().assignment(_))
  test("Expr is assignment to 32") {
    assert(a === Assign("foo", Number(32)))
  }

  val Parsed.Success(fn, _) =
    parse("def foo :: a => 1 + 4", new Parser().fn_decl(_))
  test("Function declaration") {
    assert(fn === Func("foo", "a", List(Binary(Number(1), "+", Number(4)))))
  }

  val Parsed.Success(fn_, _) =
    parse("foo(10)", new Parser().call(_))
  test("Funcion call") {
    assert(fn_ === Call("foo", List(Number(10))))
  }
}
