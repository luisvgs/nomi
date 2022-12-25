sealed trait Value {
  def +(that: Value): Value = {
    this match {
      case Integer(x) =>
        that match {
          case Integer(y) => Integer(x + y)
          case _          => ???
        }
      case _ => ???
    }
  }
  def ==(that: Value): Value = {
    this match {
      case Integer(x) =>
        that match {
          case Integer(y) => Booli(x == y)
          case _          => ???
        }
      case _ => ???
    }
  }

  def ||(that: Value): Value = {
    this match {
      case Booli(x) =>
        that match {
          case Booli(y) => Booli(x || y)
          case _          => ???
        }
      case _ => ???
    }
  }

  def <(that: Value): Value = {
    this match {
      case Integer(x) =>
        that match {
          case Integer(y) => Booli(x <= y)
          case _          => ???
        }
      case _ => ???
    }
  }

  def >(that: Value): Value = {
    this match {
      case Integer(x) =>
        that match {
          case Integer(y) => Booli(x >= y)
          case _          => ???
        }
      case _ => ???
    }
  }

  def &&(that: Value): Value = {
    this match {
      case Booli(x) =>
        that match {
          case Booli(y) => Booli(x && y)
          case _          => ???
        }
      case _ => ???
    }
  }

  def -(that: Value): Value = {
    this match {
      case Integer(x) =>
        that match {
          case Integer(y) => Integer(x - y)
          case _          => ???
        }
      case _ => ???
    }

  }

  override def toString: String = {
    this match {
      case Nothing()     => "Nothing"
      case Integer(x)    => x.toString
      case Booli(b)      => b.toString
      case StrLiteral(s) => s
    }
  }
}
case class Nothing() extends Value
case class Booli(b: Boolean) extends Value
case class Integer(x: Int) extends Value
case class StrLiteral(s: String) extends Value
