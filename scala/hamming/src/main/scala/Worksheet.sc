object Worksheet {
val s = Seq[Int]()                                //> s  : Seq[Int] = List()
s :+ 1                                            //> res0: Seq[Int] = List(1)
s                                                 //> res1: Seq[Int] = List()
//'4'.asDigit
//(0 until 0) map println
//'A'.isLower
//val m = Map('a' ->1)
//m ++ m
val str = "abcde"                                 //> str  : String = abcde
str.toSeq match {
  case 'a' +: t => t
}                                                 //> res2: Seq[Char] = bcde
val digits: Seq[Char] = "a1b2c3" filter (_.isDigit)
                                                  //> digits  : Seq[Char] = 123
digits                                            //> res3: Seq[Char] = 123
}