object Main {

  import scala.io.Source.fromFile
  import fastparse._
  import scala.io.StdIn.readLine

  def main(args: Array[String]): Unit = {
    args match {
      case Array(file) => ???
      case _ => run()
    }
  }

  def run(): Unit = {
    import scala.collection.mutable.Map
    var env = new Environment(vals = Map.empty, enclosing = None)
    var interpreter = new Interpreter(env = env)
    val source = "1 + 1"
    var tokens: Array[String] = Array()

    //tokens = source.split("\\s+")
    var p@Parsed.Success(_, _)= parse(source, new Parser().statement(_))
    interpreter.eval(p)
/*
    var continue: Boolean = true
    while (continue) {
      print("> ")
      var line = readLine()
      tokens = line.split("\\s+")

      if (line == ":q") continue = false
      var p: List[Expr]= parse(line, new Parser().statement(_)) match {
        case Parsed.Success(_,_) => _
        case _ => _
      }
      println(p)
      //var res = interpreter.eval(p)
    }
*/
  }

  def runFromFile(file: String) = {
    ???
  }

  sealed trait Operator

  case object Plus extends Operator

  case object Div extends Operator

  case object Minus extends Operator
}
