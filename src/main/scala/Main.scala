object Main {
  import fastparse._
  sealed trait Operator
  case object Plus extends Operator
  case object Div extends Operator
  case object Minus extends Operator

  def main(args: Array[String]): Unit = {
    val p = parse("false", new Parser().bool(_))
    println(p)
  }
}
