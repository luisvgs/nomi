object Main {

  import scala.io.Source.fromFile
  import fastparse._
  import scala.io.StdIn.readLine

  def main(args: Array[String]): Unit = {
    args match {
      case Array(file) => runFromFile(file)
      case _           => run()
    }
  }

  def run(): Unit = {
    import scala.collection.mutable.Map
    var env = new Environment(vals = Map.empty, enclosing = None)
    var interpreter = new Interpreter(env = env)
    // val source = """let foo = 12
    // """
    var tokens: Array[String] = Array()

    // tokens = source.split("\\s+")
    // val Parsed.Success(value, _)= parse(source, new Parser().statement(_))
    // val res = interpreter.eval(value)

    // println(res)

    var continue: Boolean = true
    while (continue) {
      print("> ")
      var line = readLine()
      tokens = line.split("\\s+")

      if (line == ":q") continue = false
      val Parsed.Success(value, _) = parse(line, new Parser().statement(_))
      var res = interpreter.eval(value)
      println(res)
    }

  }

  def runFromFile(file: String) = {}
}
