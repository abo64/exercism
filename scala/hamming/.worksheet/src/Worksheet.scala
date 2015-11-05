object Worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(37); 
val s = Seq[Int]();System.out.println("""s  : Seq[Int] = """ + $show(s ));$skip(7); val res$0 = 
s :+ 1;System.out.println("""res0: Seq[Int] = """ + $show(res$0));$skip(2); val res$1 = 
s;System.out.println("""res1: Seq[Int] = """ + $show(res$1));$skip(104); 
//'4'.asDigit
//(0 until 0) map println
//'A'.isLower
//val m = Map('a' ->1)
//m ++ m
val str = "abcde";System.out.println("""str  : String = """ + $show(str ));$skip(41); val res$2 = 
str.toSeq match {
  case 'a' +: t => t
};System.out.println("""res2: Seq[Char] = """ + $show(res$2));$skip(52); 
val digits: Seq[Char] = "a1b2c3" filter (_.isDigit);System.out.println("""digits  : Seq[Char] = """ + $show(digits ));$skip(7); val res$3 = 
digits;System.out.println("""res3: Seq[Char] = """ + $show(res$3))}
}
