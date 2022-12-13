class Environment(vals: scala.collection.mutable.Map[String, Value], enclosing: Option[Environment]){
  def define(name: String, value: Value): Unit = {
    this.vals += (name -> value)
  }

  def resolve(name: String): Option[Value] = this.vals.get(name) match {
    case Some(value) => Some(value)
    case None => this.enclosing match {
      case Some(enclosing) => return enclosing.resolve(name)
      case None => None
    }
  }
}
