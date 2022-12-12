sealed trait Value {
  def +(that: Value): Value = {
    this match {
      case Integer(x) => that match {
        case Integer(y) => Integer(x + y)
        case _ => ???
      }
      case _ => ???
    }
  }
  def -(that: Value): Value = {
    this match {
      case Integer(x) => that match {
        case Integer(y) => Integer(x - y)
        case _ => ???
      }
      case _ => ???
    }

  }
}
case class Nothing() extends Value
case class Booli(b: Boolean) extends Value
case class Integer(x: Int) extends Value

