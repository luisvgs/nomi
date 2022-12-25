object Main {
  import scala.io.Source.fromFile
  import fastparse._
  import scala.io.StdIn.readLine
  import scala.collection.mutable.Map

  def main(args: Array[String]): Unit = {
    args match {
      case Array(file) => runFromFile(file)
      case _           => run()
    }
  }

  def run(): Unit = {
    var env = new Environment(vals = Map.empty, enclosing = None)
    var interpreter = new Interpreter(env = env)
    var continue: Boolean = true
    while (continue) {
      print("> ")
      var line: String = readLine()
      line.split("\\s+")

      if (line == ":q") continue = false
      val Parsed.Success(value, _) = parse(line, new Parser().statement(_))
      var res = interpreter.eval(value)
      println(res)
    }

  }

  def runFromFile(file: String): Unit = {
    var env = new Environment(vals = Map.empty, enclosing = None)
    var interpreter = new Interpreter(env = env)
    val source = scala.io.Source.fromResource("input.nom").getLines()
     val Parsed.Success(value, _)= parse(source, new Parser().statement(_))
     val res = interpreter.eval(value)

    println(source)
  }
}
