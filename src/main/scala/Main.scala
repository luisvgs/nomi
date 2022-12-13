object Main {
  import scala.io.Source.fromFile
  import fastparse._
  import scala.io.StdIn.readLine

  sealed trait Operator

  case object Plus extends Operator

  case object Div extends Operator

  case object Minus extends Operator

  def main(args: Array[String]): Unit = {
    args match {
      case Array(file) => ???
      case _ => run()
    }
  }

  def runFromFile(file: String) = {
    ???
  }

  def run() : Unit = {
    var continue: Boolean = true
    var tokens: Array[String] = Array()
    while (continue) {
      print("> ")
      var line = readLine()
      tokens = line.split("\\s+")

      if (line == ":q") continue = false
      val p = parse(line, new Parser().factor(_))
      println(p)
    }
  }
}
