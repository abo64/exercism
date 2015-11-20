package object implicits {

  def addI(x: Int)(implicit y: Int): Int = x + y

  implicit val n = 42
//  implicit val n1 = 43

  trait Adder[T] { def add(t1: T, t2: T): T }

  def addT[T](t1: T, t2: T)(implicit adder: Adder[T]):T =
    adder.add(t1, t2)

  implicit val intAdder = new Adder[Int] {
    override def add(t1: Int, t2: Int): Int = t1 + t2
  }

//  addT(1, 2)

  implicit object StringAdder extends Adder[String] {
    override def add(t1: String, t2: String): String = t1 + t2
  }

//  addT("one", "two")

  def addT1[T: Adder](t1: T, t2: T):T =
    implicitly[Adder[T]].add(t1, t2)

//  addT1(1, 2)
//  addT1("one", "two")
//
  implicit class StringOps(str: String) {
    def add(any: Any) = str + any
  }

//  "added ".add(42)
//  "added ".add("forty" -> "two")

  type Strand = String
  
  implicit class StrandOps(str: Strand) {
    def duplicate(any: Any) = str + str
  }
  
// {
//implicit val bar = 666
//addI(1)
//}
}