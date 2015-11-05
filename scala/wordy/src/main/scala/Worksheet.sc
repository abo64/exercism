object Worksheet {
  "42" match {
    case x @ Number(n) => s"$x - $n"
    case _ => "???"
  }                                               //> res0: String = 42 - <function1>
}